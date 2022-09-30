-- | Postgres events with state as an IORef
module DomainDriven.Persistance.Postgres.Internal where

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
import           Data.Typeable
import           Database.PostgreSQL.Simple                   as PG
import qualified Database.PostgreSQL.Simple.Cursor            as Cursor
import           DomainDriven.Internal.Class
import           DomainDriven.Persistance.Postgres.Types
import           GHC.Generics                   ( Generic )
import           Lens.Micro                     ( (^.)
                                                , to
                                                )
import           Prelude
import qualified Streamly.Data.Unfold                         as Unfold
import qualified Streamly.Prelude                             as S
import           UnliftIO                       ( MonadUnliftIO(..) )

data PostgresEvent model event = PostgresEvent
    { getConnection  :: IO Connection
    , eventTableName :: EventTableName
    , modelIORef     :: IORef (NumberedModel model)
    , app            :: model -> Stored event -> model
    , seed           :: model
    , chunkSize      :: ChunkSize -- ^ Number of events read from postgres per batch
    }
    deriving Generic

data PostgresEventTrans model event = PostgresEventTrans
    { transaction    :: OngoingTransaction
    , eventTableName :: EventTableName
    , modelIORef     :: IORef (NumberedModel model)
    , app            :: model -> Stored event -> model
    , seed           :: model
    , chunkSize      :: ChunkSize -- ^ Number of events read from postgres per batch
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

    getEventStream pg = withStreamReadTransaction pg getEventStream'

getEventTableName :: EventTable -> EventTableName
getEventTableName = go 0
  where
    go :: Int -> EventTable -> String
    go i = \case
        MigrateUsing _ u -> go (i + 1) u
        InitialVersion n -> n <> "_v" <> show (i + 1)




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
           \, event_number bigint not null generated always as identity\
           \, timestamp timestamptz not null default now()\
           \, event jsonb not null\
           \);"

retireTable :: Connection -> EventTableName -> IO ()
retireTable conn tableName = do
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
    pg <- createPostgresPersistance getConn (getEventTableName eventTable) app' seed'
    withIOTrans pg $ \pgt -> runMigrations (pgt ^. field @"transaction") eventTable
    pure pg

newtype Exists = Exists
    { exists :: Bool
    }
    deriving (Show, Eq, Generic)
    deriving anyclass (FromRow)

runMigrations :: OngoingTransaction -> EventTable -> IO ()
runMigrations trans et = case et of
    InitialVersion _        -> createTable
    MigrateUsing mig prevEt -> do
        r <- query
            conn
            "select exists (select * from information_schema.tables where table_schema='public' and table_name=?)"
            (Only $ getEventTableName et)
        case r of
            [Exists True ] -> pure () -- Table exists. Nothing needs to be done
            [Exists False] -> do
                runMigrations trans prevEt
                createTable
                mig (getEventTableName prevEt) (getEventTableName et) conn
                retireTable conn (getEventTableName prevEt)

            l -> fail $ "runMigrations unexpected answer: " <> show l
  where
    conn :: Connection
    conn = fromTrans trans

    createTable :: IO ()
    createTable = do
        let tableName = getEventTableName et
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
    traverse fromEventRow =<< query_ conn q
  where
    q :: PG.Query
    q =
        "select id, event_number,timestamp,event from \""
            <> fromString eventTable
            <> "\" order by event_number"



queryEventsAfter
    :: (Typeable a, FromJSON a)
    => Connection
    -> EventTableName
    -> EventNumber
    -> IO [(Stored a, EventNumber)]
queryEventsAfter conn eventTable (EventNumber lastEvent) =
    traverse fromEventRow =<< query_
        conn
        (  "select id, event_number,timestamp,event from \""
        <> fromString eventTable
        <> "\" where event_number > "
        <> fromString (show lastEvent)
        <> " order by event_number"
        )

newtype EventQuery = EventQuery {getPgQuery:: PG.Query}
    deriving (Show, Generic)

mkEventsAfterQuery :: EventTableName -> EventNumber -> EventQuery
mkEventsAfterQuery eventTable (EventNumber lastEvent) =
    EventQuery
        $  "select id, event_number,timestamp,event from \""
        <> fromString eventTable
        <> "\" where event_number > "
        <> fromString (show lastEvent)
        <> " order by event_number"

mkEventQuery :: EventTableName -> EventQuery
mkEventQuery eventTable =
    EventQuery
        $  "select id, event_number,timestamp,event from \""
        <> fromString eventTable
        <> "\" order by event_number"

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
            <> "\" where event_number > "
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
        ("select coalesce(max(event_number),1) from \"" <> fromString eventTable <> "\"")


getEventStream'
    :: FromJSON event => PostgresEventTrans model event -> S.SerialT IO (Stored event)
getEventStream' pgt = S.map fst $ mkEventStream
    (pgt ^. field @"chunkSize")
    (pgt ^. field @"transaction")
    (pgt ^. field @"eventTableName" . to mkEventQuery)



-- | A transaction that is always rolled back at the end.
-- This required because cursors require a running transaction.
withStreamReadTransaction
    :: forall t m a model event
     . (S.IsStream t, S.MonadAsync m, MonadCatch m)
    => PostgresEvent model event
    -> (PostgresEventTrans model event -> t m a)
    -> t m a
withStreamReadTransaction pg f = S.bracket startTrans rollbackTrans f
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
        -- Nothing changes. We just need the transaction to be able to stream events.
        PG.rollback $ pgt ^. field @"transaction" . to fromTrans

withIOTrans
    :: forall a model event
     . PostgresEvent model event
    -> (PostgresEventTrans model event -> IO a)
    -> IO a
withIOTrans pg f = do
    bracket prepareTransaction cleanup $ \pgt -> do
        let conn = pgt ^. field @"transaction" . to fromTrans :: Connection
        -- We start the transaction inside the bracket so that it gets cleaned up
        PG.begin conn
        migrationResult <- try @_ @SomeException $ do
            a <- f pgt
            PG.commit conn
            pure a
        case migrationResult of
            Left e -> do
                _ <- PG.rollback conn
                throwM e
            Right a -> pure a
  where
    cleanup :: PostgresEventTrans model event -> IO ()
    cleanup pgt = do
        let conn = pgt ^. field @"transaction" . to fromTrans
        inRunningTransaction <- PG.query_ @(Only Bool)
            conn
            "SELECT now() = statement_timestamp()"
            -- `pg_current_xact_id_if_assigned()` will be null if a transaction has
            -- started but nothing has been written.
        case inRunningTransaction of
            Only True : _ -> PG.rollback conn
            _             -> pure ()

    prepareTransaction :: IO (PostgresEventTrans model event)
    prepareTransaction = do
        conn <- getConnection pg
        pure $ PostgresEventTrans { transaction    = OngoingTransaction conn
                                  , eventTableName = pg ^. field @"eventTableName"
                                  , modelIORef     = pg ^. field @"modelIORef"
                                  , app            = pg ^. field @"app"
                                  , seed           = pg ^. field @"seed"
                                  , chunkSize      = pg ^. field @"chunkSize"
                                  }



mkEventStream
    :: FromJSON event
    => ChunkSize
    -> OngoingTransaction
    -> EventQuery
    -> S.SerialT IO (Stored event, EventNumber)
mkEventStream chunkSize (OngoingTransaction conn) q = do
    let step :: Cursor.Cursor -> IO (Maybe (Seq EventRowOut, Cursor.Cursor))
        step cursor = do
            r <- Cursor.foldForward cursor chunkSize (\a r -> pure (a :|> r)) Seq.Empty
            case r of
                Left  Seq.Empty -> pure Nothing
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
        eventStream = mkEventStream
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
