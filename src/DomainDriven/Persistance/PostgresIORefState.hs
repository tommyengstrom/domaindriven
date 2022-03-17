-- | Postgres events with state as an IORef
module DomainDriven.Persistance.PostgresIORefState where

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.IORef
import           Data.Int
import           Data.List                      ( foldl' )
import           Data.String
import           Data.Text                      ( Text )
import           Data.Time
import           Data.Typeable
import           Data.UUID                      ( UUID )
import           Database.PostgreSQL.Simple                   as PG
import qualified Database.PostgreSQL.Simple.Cursor            as Cursor
import           Database.PostgreSQL.Simple.FromField         as FF
import           Database.PostgreSQL.Simple.FromRow           as FR
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

--toEventRow :: ToJSON e => Stored e -> EventRowIn
--toEventRow (Stored e ts k) = EventRow k ts (toJSON e)

--instance FromRow EventRowOut where
--    fromRow = EventRowOut <$> field <*> field <*> field <*> fieldWith fromJSONField

instance FromField EventNumber where
    fromField f bs = EventNumber <$> fromField f bs

data StateRow m = StateRow
    { modelName :: Text
    , timestamp :: UTCTime
    , state     :: m
    }
    deriving (Show, Eq, Ord, Generic)

instance (Typeable m, FromJSON m) => FromRow (StateRow m) where
    fromRow = StateRow <$> field <*> field <*> fieldWith fromJSONField

fromStateRow :: StateRow s -> s
fromStateRow = state

-- | Create the table required for storing state and events, if they do not yet exist.
createEventTable :: (FromJSON e, Typeable e) => PostgresEvent m e -> IO ()
createEventTable runner = do
    conn <- getConnection runner
    void (getModel runner)
        `catch` (const @_ @SqlError $ do
                    let etName = eventTableName runner
                    _ <- createEventTable' conn etName
                    void $ refreshModel conn runner
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
    --void $ execute
    --    conn
    --    "create trigger retired before insert on ? execute procedure retired_table()"
    --    (Only $ show tableName)
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
    :: (FromJSON e, Typeable e, Typeable m, ToJSON e, FromJSON m, ToJSON m)
    => IO Connection
    -> EventTableName
    -> (m -> Stored e -> m)
    -> m
    -> IO (PostgresEvent m e)
postgresWriteModelNoMigration getConn eventTable app' seed' = do
    runner <- createPostgresPersistance getConn eventTable app' seed'
    createEventTable runner
    pure runner


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
    ref <- newIORef (seed', 0)
    pure $ PostgresEvent { getConnection  = getConn
                         , eventTableName = eventTable
                         , modelIORef     = ref
                         , app            = app'
                         , seed           = seed'
                         , chunkSize      = 500
                         }



queryEvents
    :: (Typeable a, FromJSON a)
    => Connection
    -> EventTableName
    -> IO [(Stored a, EventNumber)]
queryEvents conn eventTable = traverse fromEventRow =<< query_
    conn
    (  "select id, commit_number,timestamp,event from \""
    <> fromString eventTable
    <> "\" order by commit_number"
    )


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


-- queryState' :: (FromJSON a, Typeable a) => Connection -> StateTableName -> IO [a]
-- queryState' conn stateTable = fmap fromStateRow
--     <$> query_ conn ("select * from \"" <> fromString stateTable <> "\"")
--
-- lockState' :: Connection -> StateTableName -> IO Int64
-- lockState' conn stateTable =
--     execute_ conn ("lock \"" <> fromString stateTable <> "\" in exclusive mode")


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

-- | Keep the events and state in postgres!
data PostgresEvent model event = PostgresEvent
    { getConnection  :: IO Connection
    , eventTableName :: EventTableName
    , modelIORef     :: IORef (model, EventNumber)
    , app            :: model -> Stored event -> model
    , seed           :: model
    , chunkSize      :: Int -- ^ Number of events read from pg per batch
    }
    deriving Generic


instance (FromJSON e, Typeable e) => ReadModel (PostgresEvent m e) where
    type Model (PostgresEvent m e) = m
    type Event (PostgresEvent m e) = e
    applyEvent pg = app pg
    getModel pg = do
        conn <- getConnection pg
        withTransaction conn $ getModel' conn pg

    getEventList pg = do
        conn <- getConnection pg
        fmap fst <$> queryEvents conn (eventTableName pg)

    getEventStream pg = do
        S.mapM (fmap fst . fromEventRow) $ S.unfoldMany Unfold.fromList $ mkEventStream
            (chunkSize pg)
            (getConnection pg)
            (eventTableName pg)


mkEventStream :: Int -> IO Connection -> EventTableName -> S.SerialT IO [EventRowOut]
mkEventStream chunkSize getConn et = do
    conn <- liftIO getConn
    -- FIXME: My gut tells me I may have some issue here as I'm starting a transaction and
    -- potentially this could fail and linger. Normally, if this was in IO, I could
    -- just wrap it in a bracket but here I cannot.
    -- Is this an issue? How do I fix it?
    liftIO $ PG.begin conn
    let step :: Cursor.Cursor -> IO (Maybe ([EventRowOut], Cursor.Cursor))
        step cursor = do
            r <- Cursor.foldForward cursor chunkSize (\a r -> pure (a <> [r])) []
            case r of
                Left  [] -> Nothing <$ liftIO (PG.rollback conn)
                Left  a  -> pure $ Just (a, cursor)
                Right a  -> pure $ Just (a, cursor)
        eventQuery :: EventTableName -> PG.Query
        eventQuery eventTable =
            "select id, commit_number,timestamp,event from \""
                <> fromString eventTable
                <> "\" order by commit_number"


    cursor <- liftIO $ Cursor.declareCursor conn (eventQuery et)
    S.unfoldrM step cursor


getModel'
    :: forall e m . (FromJSON e, Typeable e) => Connection -> PostgresEvent m e -> IO m
getModel' conn pg = do
    (model, lastEventNo) <- readIORef (modelIORef pg)
    newEvents            <- queryEventsAfter @e conn (eventTableName pg) lastEventNo
    case newEvents of
        [] -> pure model
        _  -> fst <$> refreshModel conn pg -- The model must be updated within a transaction

refreshModel
    :: (Typeable e, FromJSON e) => Connection -> PostgresEvent m e -> IO (m, EventNumber)
refreshModel conn pg = do
    -- refresh doesn't write any events but changes the state and thus needs a lock
    exclusiveLock conn (eventTableName pg)
    (model    , lastEventNo   ) <- readIORef (modelIORef pg)
    (newEvents, lastNewEventNo) <- do
        (evs, nos) <- unzip <$> queryEventsAfter conn (eventTableName pg) lastEventNo
        pure (evs, foldl' max 0 nos)
    case newEvents of
        [] -> pure (model, lastEventNo)
        _  -> do
            let newModel = foldl' (app pg) model newEvents
            _ <- writeIORef (modelIORef pg) (newModel, lastNewEventNo)
            pure (newModel, lastNewEventNo)

withExclusiveLock :: PostgresEvent m e -> (Connection -> IO a) -> IO a
withExclusiveLock pg f = do
    conn <- getConnection pg
    withTransaction conn $ do
        exclusiveLock conn (eventTableName pg)
        f conn

exclusiveLock :: Connection -> EventTableName -> IO ()
exclusiveLock conn etName =
    () <$ execute_ conn ("lock \"" <> fromString etName <> "\" in exclusive mode")

instance (ToJSON e, FromJSON e, Typeable e) => WriteModel (PostgresEvent m e) where
    transactionalUpdate pg cmd = withRunInIO $ \runInIO -> do
        conn <- getConnection pg
        withTransaction conn $ do
            let eventTable = eventTableName pg
            _ <- execute_
                conn
                ("lock \"" <> fromString eventTable <> "\" in exclusive mode")
            m                <- getModel' conn pg
            (returnFun, evs) <- runInIO $ cmd m
            m'               <- fst <$> readIORef (modelIORef pg)
            storedEvs        <- traverse toStored evs
            let newM = foldl' (app pg) m' storedEvs
            lastEventNo <- writeEvents conn eventTable storedEvs
            _           <- writeIORef (modelIORef pg) (newM, lastEventNo)
            pure $ returnFun newM

migrateValue1to1
    :: Connection -> PreviousEventTableName -> EventTableName -> (Value -> Value) -> IO ()
migrateValue1to1 conn prevTName tName f = migrate1to1 conn prevTName tName (fmap f)

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
