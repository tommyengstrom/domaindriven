-- | Postgres events with state as an IORef
module DomainDriven.Persistance.PostgresIORefState where

import           Control.Lens                   ( (^.)
                                                , to
                                                )
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Foldable
import           Data.Generics.Product
import           Data.IORef
import           Data.Int
import qualified Data.Sequence                                as Seq
import           Data.Sequence                  ( Seq(..) )
import           Data.String
import           Data.Text                      ( Text )
import           Data.Time
import           Data.Typeable
import           Data.UUID                      ( UUID )
import           Database.PostgreSQL.Simple                   as PG
import qualified Database.PostgreSQL.Simple.Cursor            as Cursor
import           Database.PostgreSQL.Simple.FromField         as FF
import           DomainDriven.Internal.Class
import           GHC.Generics                   ( Generic )
import           Prelude
import qualified Streamly.Data.Unfold                         as Unfold
import qualified Streamly.Prelude                             as S
import           UnliftIO                       ( MonadUnliftIO(..) )


data PersistanceError
    = EncodingError String
    | ValueError String
    deriving (Show, Eq, Typeable, Exception)

type EventTableBaseName = String
type EventVersion = Int
type EventTableName = String
type PreviousEventTableName = String


data EventTable
    = MigrateUsing EventMigration EventTable
    | InitialVersion EventTableBaseName

type EventMigration = PreviousEventTableName -> EventTableName -> Connection -> IO ()

data PostgresEvent model event = PostgresEvent
    { getConnection  :: IO Connection
    , eventTableName :: EventTableName
    , modelIORef     :: IORef (NumberedModel model)
    , app            :: model -> Stored event -> model
    , seed           :: model
    , chunkSize      :: Int -- ^ Number of events read from postgres per batch
    }
    deriving Generic

data PostgresEventTrans model event = PostgresEventTrans
    { transaction    :: OngoingTransaction
    , eventTableName :: EventTableName
    , modelIORef     :: IORef (NumberedModel model)
    , app            :: model -> Stored event -> model
    , seed           :: model
    , chunkSize      :: Int -- ^ Number of events read from postgres per batch
    }
    deriving Generic


instance (FromJSON e, Typeable e) => ReadModel (PostgresEvent m e) where
    type Model (PostgresEvent m e) = m
    type Event (PostgresEvent m e) = e
    applyEvent pg = pg ^. field @"app"
    getModel pg = withIOTrans pg getModel'

    getEventList pg = do
        conn <- getConnection pg
        fmap fst <$> queryEvents conn (pg ^. field @"eventTableName")

    getEventStream pg = withStreamTrans pg getEventStream'

getEventTableName :: EventTable -> EventTableName
getEventTableName = go 0
  where
    go :: Int -> EventTable -> String
    go i = \case
        MigrateUsing _ u -> go (i + 1) u
        InitialVersion n -> n <> "_v" <> show (i + 1)


newtype EventNumber = EventNumber {unEventNumber :: Int64}
    deriving (Show, Generic)
    deriving newtype (Eq, Ord, Num)

decodeEventRow :: (UUID, UTCTime, e) -> Stored e
decodeEventRow (k, ts, e) = Stored e ts k

data EventRowOut = EventRowOut
    { key          :: UUID
    , commitNumber :: EventNumber
    , timestamp    :: UTCTime
    , event        :: Value
    }
    deriving (Show, Eq, Generic, FromRow)

fromEventRow :: (FromJSON e, MonadThrow m) => EventRowOut -> m (Stored e, EventNumber)
fromEventRow (EventRowOut k no ts ev) = case fromJSON ev of
    Success a -> pure (Stored a ts k, no)
    Error err ->
        throwM
            .  EncodingError
            $  "Failed to parse event "
            <> show k
            <> ": "
            <> err
            <> "\nWhen trying to parse:\n"
            <> show ev

instance FromField EventNumber where
    fromField f bs = EventNumber <$> fromField f bs

data StateRow m = StateRow
    { modelName :: Text
    , timestamp :: UTCTime
    , state     :: m
    }
    deriving (Show, Eq, Ord, Generic)


-- | Create the table required for storing state and events, if they do not yet exist.
createEventTable
    :: (FromJSON e, Typeable e, WriteModel (PostgresEventTrans m e))
    => PostgresEventTrans m e
    -> IO ()
createEventTable pgt = do
    void (getModel pgt)
        `catch` (const @_ @SqlError $ do
                    let etName = pgt ^. field @"eventTableName"
                    _ <- createEventTable'
                        (pgt ^. field @"transaction" . to fromTrans)
                        etName
                    void $ refreshModel pgt
                )
createEventTable' :: Connection -> EventTableName -> IO Int64
createEventTable' conn eventTable =
    execute_ conn
        $ "create table if not exists \""
        <> fromString eventTable
        <> "\" \
           \( id uuid primary key\
           \, commit_number bigint not null generated always as identity\
           \, timestamp timestamptz not null default now()\
           \, event jsonb not null\
           \);"

retireTable :: Connection -> EventTableName -> IO ()
retireTable conn tableName = do
    putStrLn $ "Retiering: " <> show tableName
    createRetireFunction conn
    void
        $  execute_ conn
        $  "create trigger retired before insert on \""
        <> fromString tableName
        <> "\" execute procedure retired_table()"

createRetireFunction :: Connection -> IO ()
createRetireFunction conn =
    void
        . execute_ conn
        $ "create or replace function retired_table() returns trigger as \
                    \$$ begin raise exception 'Event table has been retired.'; end; $$ \
                    \language plpgsql;"

-- | Setup the persistance model and verify that the tables exist.
postgresWriteModelNoMigration
    :: (FromJSON e, ToJSON e, Typeable e, Typeable m, WriteModel (PostgresEventTrans m e))
    => IO Connection
    -> EventTableName
    -> (m -> Stored e -> m)
    -> m
    -> IO (PostgresEvent m e)
postgresWriteModelNoMigration getConn eventTable app' seed' = do
    pg <- createPostgresPersistance getConn eventTable app' seed'
    withIOTrans pg createEventTable
    pure pg


-- | Setup the persistance model and verify that the tables exist.
postgresWriteModel
    :: (FromJSON e, Typeable e, ToJSON e)
    => IO Connection
    -> EventTable
    -> (m -> Stored e -> m)
    -> m
    -> IO (PostgresEvent m e)
postgresWriteModel getConn eventTable app' seed' = do
    conn <- getConn
    withTransaction conn $ runMigrations conn eventTable
    createPostgresPersistance getConn (getEventTableName eventTable) app' seed'

newtype Exists = Exists
    { exists :: Bool
    }
    deriving (Show, Eq, Generic)
    deriving anyclass (FromRow)

runMigrations :: Connection -> EventTable -> IO ()
runMigrations conn et = case et of
    InitialVersion _        -> createTable
    MigrateUsing mig prevEt -> do
        r <- query
            conn
            "select exists (select * from information_schema.tables where table_schema='public' and table_name=?)"
            (Only $ getEventTableName et)
        case r of
            [Exists True ] -> pure () -- Table exists. Nothing needs to be done
            [Exists False] -> do
                runMigrations conn prevEt
                createTable
                mig (getEventTableName prevEt) (getEventTableName et) conn
                retireTable conn (getEventTableName prevEt)

            l -> fail $ "runMigrations unexpected answer: " <> show l
  where
    createTable :: IO ()
    createTable = do
        let tableName = getEventTableName et
        putStrLn $ "runMigrations: Creating table: " <> show tableName
        void $ createEventTable' conn tableName


nextEventTableVersionExists :: Connection -> EventTable -> IO Bool
nextEventTableVersionExists conn et = do
    r <- query
        conn
        "select exists (select * from information_schema.tables where table_schema='public' and table_name=?)"
        (Only $ getEventTableName $ MigrateUsing (\_ _ _ -> pure ()) et)
    case r of
        [Exists v] -> pure v
        l          -> fail $ "nextEventTableVersionExists unexpected answer: " <> show l


createPostgresPersistance
    :: forall event model
     . (FromJSON event, Typeable event, ToJSON event)
    => IO Connection
    -> EventTableName
    -> (model -> Stored event -> model)
    -> model
    -> IO (PostgresEvent model event)
createPostgresPersistance getConn eventTable app' seed' = do
    ref <- newIORef $ NumberedModel seed' 0
    pure $ PostgresEvent { getConnection  = getConn
                         , eventTableName = eventTable
                         , modelIORef     = ref
                         , app            = app'
                         , seed           = seed'
                         , chunkSize      = 50
                         }



queryEvents
    :: (Typeable a, FromJSON a)
    => Connection
    -> EventTableName
    -> IO [(Stored a, EventNumber)]
queryEvents conn eventTable = do
    putStrLn "-------- Running queryEvents"
    traverse fromEventRow =<< query_ conn q
  where
    q :: PG.Query
    q =
        "select id, commit_number,timestamp,event from \""
            <> fromString eventTable
            <> "\" order by commit_number"



queryEventsAfter
    :: (Typeable a, FromJSON a)
    => Connection
    -> EventTableName
    -> EventNumber
    -> IO [(Stored a, EventNumber)]
queryEventsAfter conn eventTable (EventNumber lastEvent) =
    traverse fromEventRow =<< query_
        conn
        (  "select id, commit_number,timestamp,event from \""
        <> fromString eventTable
        <> "\" where commit_number > "
        <> fromString (show lastEvent)
        <> " order by commit_number"
        )

newtype EventQuery = EventQuery {getPgQuery:: PG.Query}
    deriving (Show, Generic)

mkEventsAfterQuery :: EventTableName -> EventNumber -> EventQuery
mkEventsAfterQuery eventTable (EventNumber lastEvent) =
    EventQuery
        $  "select id, commit_number,timestamp,event from \""
        <> fromString eventTable
        <> "\" where commit_number > "
        <> fromString (show lastEvent)
        <> " order by commit_number"

mkEventQuery :: EventTableName -> EventQuery
mkEventQuery eventTable =
    EventQuery
        $  "select id, commit_number,timestamp,event from \""
        <> fromString eventTable
        <> "\" order by commit_number"

headMay :: [a] -> Maybe a
headMay = \case
    a : _ -> Just a
    []    -> Nothing

queryHasEventsAfter :: Connection -> EventTableName -> EventNumber -> IO Bool
queryHasEventsAfter conn eventTable (EventNumber lastEvent) =
    maybe True fromOnly . headMay <$> query_ conn q
  where
    q :: PG.Query
    q =
        "select count(*) > 0 from \""
            <> fromString eventTable
            <> "\" where commit_number > "
            <> fromString (show lastEvent)

writeEvents
    :: forall a
     . (ToJSON a)
    => Connection
    -> EventTableName
    -> [Stored a]
    -> IO EventNumber
writeEvents conn eventTable storedEvents = do
    _ <- executeMany
        conn
        (  "insert into \""
        <> fromString eventTable
        <> "\" (id, timestamp, event) \
            \values (?, ?, ?)"
        )
        (fmap (\x -> (storedUUID x, storedTimestamp x, encode $ storedEvent x))
              storedEvents
        )
    foldl' max 0 . fmap fromOnly <$> query_
        conn
        ("select coalesce(max(commit_number),1) from \"" <> fromString eventTable <> "\"")


getEventStream'
    :: FromJSON event => PostgresEventTrans model event -> S.SerialT IO (Stored event)
getEventStream' pgt = S.map fst $ mkEventStream'
    (pgt ^. field @"chunkSize")
    (pgt ^. field @"transaction")
    (pgt ^. field @"eventTableName" . to mkEventQuery)


data OngoingTransaction = OngoingTransaction
    { fromTrans :: Connection
    }

withStreamTrans
    :: forall t m a model event
     . (S.IsStream t, S.MonadAsync m, MonadCatch m)
    => PostgresEvent model event
    -> (PostgresEventTrans model event -> t m a)
    -> t m a
withStreamTrans pg f = S.bracket startTrans rollbackTrans f
  where
    startTrans :: m (PostgresEventTrans model event)
    startTrans = liftIO $ do
        conn <- getConnection pg
        PG.begin conn
        pure $ PostgresEventTrans { transaction    = OngoingTransaction conn
                                  , eventTableName = pg ^. field @"eventTableName"
                                  , modelIORef     = pg ^. field @"modelIORef"
                                  , app            = pg ^. field @"app"
                                  , seed           = pg ^. field @"seed"
                                  , chunkSize      = pg ^. field @"chunkSize"
                                  }

    rollbackTrans :: PostgresEventTrans model event -> m ()
    rollbackTrans pgt = liftIO $ do
        PG.rollback $ pgt ^. field @"transaction" . to fromTrans
        undefined -- throw something


withIOTrans
    :: forall a model event
     . PostgresEvent model event
    -> (PostgresEventTrans model event -> IO a)
    -> IO a
withIOTrans pg f = bracket startTrans rollbackTrans f
  where
    startTrans :: IO (PostgresEventTrans model event)
    startTrans = do
        conn <- getConnection pg
        PG.begin conn
        pure $ PostgresEventTrans { transaction    = OngoingTransaction conn
                                  , eventTableName = pg ^. field @"eventTableName"
                                  , modelIORef     = pg ^. field @"modelIORef"
                                  , app            = pg ^. field @"app"
                                  , seed           = pg ^. field @"seed"
                                  , chunkSize      = pg ^. field @"chunkSize"
                                  }

    rollbackTrans :: PostgresEventTrans model event -> IO ()
    rollbackTrans pgt = do
        PG.rollback $ pgt ^. field @"transaction" . to fromTrans
        undefined -- throw something



mkEventStream'
    :: FromJSON event
    => Int
    -> OngoingTransaction
    -> EventQuery
    -> S.SerialT IO (Stored event, EventNumber)
mkEventStream' chunkSize (OngoingTransaction conn) q = do
    let step :: Cursor.Cursor -> IO (Maybe (Seq EventRowOut, Cursor.Cursor))
        step cursor = do
            r <- Cursor.foldForward cursor chunkSize (\a r -> pure (a :|> r)) Seq.Empty
            case r of
                Left  Seq.Empty -> Nothing <$ liftIO (PG.rollback conn)
                Left  a         -> pure $ Just (a, cursor)
                Right a         -> pure $ Just (a, cursor)


    cursor <- liftIO $ Cursor.declareCursor conn (getPgQuery q)
    S.mapM fromEventRow $ S.unfoldMany Unfold.fromList . fmap toList $ S.unfoldrM
        step
        cursor


mkEventStream
    :: FromJSON event
    => Int
    -> IO Connection
    -> EventQuery
    -> S.SerialT IO (Stored event, EventNumber)
mkEventStream chunkSize getConn q = do
    conn <- liftIO getConn
    -- FIXME: My gut tells me I may have some issue here as I'm starting a transaction and
    -- potentially this could fail and linger. Normally, if this was in IO, I could
    -- just wrap it in a bracket but here I cannot.
    -- Is this an issue? How do I fix it?
    liftIO $ PG.begin conn
    let step :: Cursor.Cursor -> IO (Maybe (Seq EventRowOut, Cursor.Cursor))
        step cursor = do
            r <- Cursor.foldForward cursor chunkSize (\a r -> pure (a :|> r)) Seq.Empty
            case r of
                Left  Seq.Empty -> Nothing <$ liftIO (PG.rollback conn)
                Left  a         -> pure $ Just (a, cursor)
                Right a         -> pure $ Just (a, cursor)


    cursor <- liftIO $ Cursor.declareCursor conn (getPgQuery q)
    S.mapM fromEventRow $ S.unfoldMany Unfold.fromList . fmap toList $ S.unfoldrM
        step
        cursor


getModel' :: forall e m . (FromJSON e, Typeable e) => PostgresEventTrans m e -> IO m
getModel' pgt = do
    NumberedModel model lastEventNo <- readIORef (pgt ^. field @"modelIORef")
    hasNewEvents <- queryHasEventsAfter (pgt ^. field @"transaction" . to fromTrans)
                                        (pgt ^. field @"eventTableName")
                                        lastEventNo
    if hasNewEvents then fst <$> refreshModel pgt else pure model

--refreshModel
--    :: (Typeable e, FromJSON e) => Connection -> PostgresEvent m e -> IO (m, EventNumber)
--refreshModel conn pg = withTransaction conn $ do
--    -- refresh doesn't write any events but changes the state and thus needs a lock
--    exclusiveLock conn (eventTableName pg)
--    NumberedModel model lastEventNo <- readIORef (modelIORef pg)
--    (newEvents, lastNewEventNo)     <- do
--        (evs, nos) <- unzip <$> queryEventsAfter conn (eventTableName pg) lastEventNo
--        pure (evs, foldl' max 0 nos)
--    case newEvents of
--        [] -> pure (model, lastEventNo)
--        _  -> do
--            let newModel = foldl' (app pg) model newEvents
--            _ <- writeIORef (modelIORef pg) $ NumberedModel newModel lastNewEventNo
--            pure (newModel, lastNewEventNo)

data NumberedModel m = NumberedModel
    { model       :: !m
    , eventNumber :: !EventNumber
    }
    deriving (Show, Generic)

data NumberedEvent e = NumberedEvent
    { event       :: !(Stored e)
    , eventNumber :: EventNumber
    }
    deriving (Show, Generic)

refreshModel
    :: forall m e
     . (Typeable e, FromJSON e)
    => PostgresEventTrans m e
    -> IO (m, EventNumber)
refreshModel pg = do
    -- refresh doesn't write any events but changes the state and thus needs a lock
    exclusiveLock (pg ^. field @"transaction") (pg ^. field @"eventTableName")
    NumberedModel model lastEventNo <- readIORef (pg ^. field @"modelIORef")
    let eventStream :: S.SerialT IO (Stored e, EventNumber)
        eventStream = mkEventStream'
            (pg ^. field @"chunkSize")
            (pg ^. field @"transaction")
            (mkEventsAfterQuery (pg ^. field @"eventTableName") lastEventNo)

        applyModel :: NumberedModel m -> (Stored e, EventNumber) -> NumberedModel m
        applyModel (NumberedModel m _) (ev, evNumber) =
            NumberedModel ((pg ^. field @"app") m ev) evNumber

    NumberedModel newModel lastNewEventNo <- S.foldl'
        applyModel
        (NumberedModel model lastEventNo)
        eventStream


    _ <- writeIORef (pg ^. field @"modelIORef") $ NumberedModel newModel lastNewEventNo
    pure (newModel, lastNewEventNo)

refreshModel''
    :: forall m e
     . (Typeable e, FromJSON e)
    => OngoingTransaction
    -> EventTableName
    -> IORef (NumberedModel m)
    -> (m -> Stored e -> m)
    -> Int
    -> IO (m, EventNumber)
refreshModel'' trans etName modelRef applyEv chunk = do
    -- refresh doesn't write any events but changes the state and thus needs a lock
    exclusiveLock trans etName
    NumberedModel model lastEventNo <- readIORef modelRef
    let eventStream :: S.SerialT IO (Stored e, EventNumber)
        eventStream = mkEventStream' chunk trans (mkEventsAfterQuery etName lastEventNo)

        applyEvNumbered :: NumberedModel m -> (Stored e, EventNumber) -> NumberedModel m
        applyEvNumbered (NumberedModel m _) (ev, evNumber) =
            NumberedModel (applyEv m ev) evNumber

    NumberedModel newModel lastNewEventNo <- S.foldl'
        applyEvNumbered
        (NumberedModel model lastEventNo)
        eventStream

    _ <- writeIORef modelRef $ NumberedModel newModel lastNewEventNo
    pure (newModel, lastNewEventNo)

--withExclusiveLock :: PostgresEvent m e -> (Connection -> IO a) -> IO a
--withExclusiveLock pg f = do
--    conn <- getConnection pg
--    withTransaction conn $ do
--        exclusiveLock conn (eventTableName pg)
--        f conn

exclusiveLock :: OngoingTransaction -> EventTableName -> IO ()
exclusiveLock (OngoingTransaction conn) etName =
    () <$ execute_ conn ("lock \"" <> fromString etName <> "\" in exclusive mode")

instance (ToJSON e, FromJSON e, Typeable e) => WriteModel (PostgresEvent m e) where
    transactionalUpdate pg cmd = withRunInIO $ \runInIO -> do
        withIOTrans pg $ \pgt -> do
            let eventTable = pg ^. field @"eventTableName"
            exclusiveLock (pgt ^. field @"transaction") eventTable
            m                  <- getModel' pgt
            (returnFun, evs)   <- runInIO $ cmd m
            NumberedModel m' _ <- readIORef (pg ^. field @"modelIORef")
            storedEvs          <- traverse toStored evs
            let newM = foldl' (pg ^. field @"app") m' storedEvs
            lastEventNo <- writeEvents (pgt ^. field @"transaction" . to fromTrans)
                                       eventTable
                                       storedEvs
            _ <- writeIORef (pg ^. field @"modelIORef") $ NumberedModel newM lastEventNo
            pure $ returnFun newM

-- migrateValue1to1
--     :: Connection -> PreviousEventTableName -> EventTableName -> (Value -> Value) -> IO ()
-- migrateValue1to1 conn prevTName tName f = migrate1to1 conn prevTName tName (fmap f)
migrate1to1'
    :: forall a b
     . (Typeable a, FromJSON a, ToJSON b, Show a)
    => Connection
    -> PreviousEventTableName
    -> EventTableName
    -> (Stored a -> Stored b)
    -> IO ()
migrate1to1' conn prevTName tName f = do
    _ <- createEventTable' conn tName
    S.mapM_ (liftIO . writeIt) $ S.map (f . fst) $ mkEventStream
        1
        (pure conn)
        (mkEventQuery prevTName)
  where
    writeIt :: Stored b -> IO Int64
    writeIt event = PG.executeMany
        conn
        (  "insert into \""
        <> fromString tName
        <> "\" (id, timestamp, event) \
                \values (?, ?, ?)"
        )
        (fmap (\x -> (storedUUID x, storedTimestamp x, encode $ storedEvent x)) [event])

migrate1to1
    :: forall a b
     . (Typeable a, FromJSON a, ToJSON b, Show a)
    => Connection
    -> PreviousEventTableName
    -> EventTableName
    -> (Stored a -> Stored b)
    -> IO ()
migrate1to1 conn prevTName tName f = do
    _             <- createEventTable' conn tName
    currentEvents <- queryEvents @a conn prevTName
    void $ writeEvents conn tName $ fmap (f . fst) currentEvents

migrate1toMany
    :: forall a b
     . (Typeable a, FromJSON a, ToJSON b)
    => Connection
    -> PreviousEventTableName
    -> EventTableName
    -> (Stored a -> [Stored b])
    -> IO EventNumber
migrate1toMany conn prevTName tName f = do
    _             <- createEventTable' conn tName
    currentEvents <- queryEvents @a conn prevTName
    let mkNewEvents :: [(Stored a, EventNumber)] -> [Stored b]
        mkNewEvents = ((f . fst) =<<)
    writeEvents conn tName $ mkNewEvents currentEvents
