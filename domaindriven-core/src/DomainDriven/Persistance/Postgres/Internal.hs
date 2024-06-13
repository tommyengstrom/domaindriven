-- | Postgres events with state as an IORef
module DomainDriven.Persistance.Postgres.Internal where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson
import Data.Foldable
import Data.Generics.Product
import Data.IORef
import Data.Int
import Data.Pool.Introspection as Pool
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.String
import Data.Time
import Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.Cursor qualified as Cursor
import DomainDriven.Persistance.Class
import DomainDriven.Persistance.Postgres.Types
import GHC.Generics (Generic)
import GHC.Stack
import Lens.Micro
    ( to
    , (^.)
    )
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Stream.Prelude (Stream)
import Streamly.Data.Stream.Prelude qualified as Stream
import Streamly.Data.Unfold qualified as Unfold
import UnliftIO (MonadUnliftIO (..), concurrently)
import Prelude

data LogEntry
    = DbTransactionDuration PrettyCallStack NominalDiffTime
    | EventTableLockDuration PrettyCallStack NominalDiffTime
    | EventTableMigrationDuration EventTableName NominalDiffTime
    | WaitForConnectionDuration PrettyCallStack NominalDiffTime
    deriving (Show, Generic)

newtype PrettyCallStack = PrettyCallStack CallStack

instance Show PrettyCallStack where
    show (PrettyCallStack c) = unwords . lines $ prettyCallStack c

data PostgresEvent model event = PostgresEvent
    { connectionPool :: Pool Connection
    , eventTableName :: EventTableName
    , modelIORef :: IORef (NumberedModel model)
    , app :: model -> Stored event -> model
    , seed :: model
    , chunkSize :: ChunkSize
    -- ^ Number of events read from postgres per batch
    , updateHook :: PostgresEvent model event -> model -> [Stored event] -> IO ()
    , logger :: LogEntry -> IO ()
    }
    deriving (Generic)

data PostgresEventTrans model event = PostgresEventTrans
    { transaction :: OngoingTransaction
    , eventTableName :: EventTableName
    , modelIORef :: IORef (NumberedModel model)
    , app :: model -> Stored event -> model
    , seed :: model
    , chunkSize :: ChunkSize
    , logger :: LogEntry -> IO ()
    -- ^ Number of events read from postgres per batch
    }
    deriving (Generic)

instance FromJSON e => ReadModel (PostgresEvent m e) where
    type Model (PostgresEvent m e) = m
    type Event (PostgresEvent m e) = e
    applyEvent pg = pg ^. field @"app"
    getModel pg = withIOTrans pg getModel'

    getEventList pg = withResource (connectionPool pg) $ \conn ->
        fmap fst <$> queryEvents (Pool.resource conn) (pg ^. field @"eventTableName")

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
    :: (FromJSON e, WriteModel (PostgresEventTrans m e))
    => PostgresEventTrans m e
    -> IO ()
createEventTable pgt = do
    void (getModel pgt)
        `catch` ( const @_ @SqlError $ do
                    let etName = pgt ^. field @"eventTableName"
                    _ <-
                        createEventTable'
                            (pgt ^. field @"transaction" . field @"connectionResource" . field @"resource")
                            etName
                    void $ refreshModel pgt
                )

createEventTable' :: Connection -> EventTableName -> IO Int64
createEventTable' conn eventTable =
    execute_ conn $
        "create table if not exists \""
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
    void $
        execute_ conn $
            "create trigger retired before insert on \""
                <> fromString tableName
                <> "\" execute procedure retired_table()"

createRetireFunction :: Connection -> IO ()
createRetireFunction conn =
    void
        . execute_ conn
        $ "create or replace function retired_table() returns trigger as \
          \$$ begin raise exception 'Event table has been retired.'; end; $$ \
          \language plpgsql;"

simplePool' :: MonadUnliftIO m => PG.ConnectInfo -> m (Pool Connection)
simplePool' connInfo = simplePool (PG.connect connInfo)

simplePool :: MonadUnliftIO m => IO Connection -> m (Pool Connection)
simplePool getConn = do
    -- Using a single stripe to ensures all thread can use all connections
    let poolCfg :: Pool.PoolConfig Connection
        poolCfg =
            Pool.setNumStripes (Just 1) $
                Pool.defaultPoolConfig (liftIO getConn) (liftIO . PG.close) 60 5

    liftIO $ Pool.newPool poolCfg

-- | Setup the persistance model and verify that the tables exist.
postgresWriteModelNoMigration
    :: HasCallStack
    => (FromJSON event, WriteModel (PostgresEventTrans model event))
    => Pool Connection
    -> EventTableName
    -> (model -> Stored event -> model)
    -> model
    -> IO (PostgresEvent model event)
postgresWriteModelNoMigration pool eventTable app' seed' = do
    pg <- createPostgresPersistance pool eventTable app' seed'
    withIOTrans pg createEventTable
    pure pg

-- | Setup the persistance model and verify that the tables exist.
postgresWriteModel
    :: HasCallStack
    => Pool Connection
    -> EventTable
    -> (model -> Stored event -> model)
    -> model
    -> IO (PostgresEvent model event)
postgresWriteModel pool eventTable app' seed' = do
    pg <- createPostgresPersistance pool (getEventTableName eventTable) app' seed'
    withIOTrans pg $ \pgt -> runMigrations (pgt ^. field @"logger") (pgt ^. field @"transaction") eventTable
    pure pg

newtype Exists = Exists
    { exists :: Bool
    }
    deriving (Show, Eq, Generic)
    deriving anyclass (FromRow)

runMigrations :: (LogEntry -> IO ()) -> OngoingTransaction -> EventTable -> IO ()
runMigrations logger trans et = do
    tableExistQuery <-
        query
            conn
            "select exists (select * from information_schema.tables where table_schema='public' and table_name=?)"
            (Only $ getEventTableName et)

    case (et, tableExistQuery) of
        (InitialVersion _, [Only True]) -> pure ()
        (MigrateUsing{}, [Only True]) -> pure ()
        (InitialVersion _, [Only False]) -> createTable
        (MigrateUsing mig prevEt, [Only False]) -> do
            -- Ensure migrations are done up until the previous table
            runMigrations logger trans prevEt
            -- Then lock lock the previous table before we start
            exclusiveLock trans (getEventTableName prevEt)
            t0 <- getCurrentTime
            createTable
            mig (getEventTableName prevEt) (getEventTableName et) conn
            retireTable conn (getEventTableName prevEt)
            t1 <- getCurrentTime
            logger $ EventTableMigrationDuration (getEventTableName et) (diffUTCTime t1 t0)
        (_, r) -> fail $ "Unexpected table query result: " <> show r
  where
    conn :: Connection
    conn = trans ^. field @"connectionResource" . field @"resource"

    createTable :: IO ()
    createTable = do
        let tableName = getEventTableName et
        void $ createEventTable' conn tableName

createPostgresPersistance
    :: forall event model
     . Pool Connection
    -> EventTableName
    -> (model -> Stored event -> model)
    -- ^ Apply event
    -> model
    -- ^ Initial model
    -> IO (PostgresEvent model event)
createPostgresPersistance pool eventTable app' seed' = do
    ref <- newIORef $ NumberedModel seed' 0
    pure $
        PostgresEvent
            { connectionPool = pool
            , eventTableName = eventTable
            , modelIORef = ref
            , app = app'
            , seed = seed'
            , chunkSize = 50
            , updateHook = \_ _ _ -> pure ()
            , logger = \case
                e@(DbTransactionDuration _ dt) -> when (dt > 1) $ putStrLn $ "[DomainDriven] " <> show e
                e@(EventTableLockDuration _ dt) -> when (dt > 0.5) $ putStrLn $ "[DomainDriven] " <> show e
                EventTableMigrationDuration etName dt -> putStrLn $ "[DomainDriven] migration of " <> etName <> " completed in " <> show dt
                e@(WaitForConnectionDuration _ dt) -> when (dt > 0.5) $ putStrLn $ "[DomainDriven] " <> show e
            }

queryEvents
    :: FromJSON a
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
    :: FromJSON a
    => Connection
    -> EventTableName
    -> EventNumber
    -> IO [(Stored a, EventNumber)]
queryEventsAfter conn eventTable (EventNumber lastEvent) =
    traverse fromEventRow
        =<< query_
            conn
            ( "select id, event_number,timestamp,event from \""
                <> fromString eventTable
                <> "\" where event_number > "
                <> fromString (show lastEvent)
                <> " order by event_number"
            )

newtype EventQuery = EventQuery {getPgQuery :: PG.Query}
    deriving (Show, Generic)

mkEventsAfterQuery :: EventTableName -> EventNumber -> EventQuery
mkEventsAfterQuery eventTable (EventNumber lastEvent) =
    EventQuery $
        "select id, event_number,timestamp,event from \""
            <> fromString eventTable
            <> "\" where event_number > "
            <> fromString (show lastEvent)
            <> " order by event_number"

mkEventQuery :: EventTableName -> EventQuery
mkEventQuery eventTable =
    EventQuery $
        "select id, event_number,timestamp,event from \""
            <> fromString eventTable
            <> "\" order by event_number"

headMay :: [a] -> Maybe a
headMay = \case
    a : _ -> Just a
    [] -> Nothing

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
     . ToJSON a
    => Connection
    -> EventTableName
    -> [Stored a]
    -> IO EventNumber
writeEvents conn eventTable storedEvents = do
    _ <-
        executeMany
            conn
            ( "insert into \""
                <> fromString eventTable
                <> "\" (id, timestamp, event) \
                   \values (?, ?, ?)"
            )
            ( fmap
                (\x -> (storedUUID x, storedTimestamp x, encode $ storedEvent x))
                storedEvents
            )
    foldl' max 0 . fmap fromOnly
        <$> query_
            conn
            ("select coalesce(max(event_number),1) from \"" <> fromString eventTable <> "\"")

getEventStream'
    :: FromJSON event => PostgresEventTrans model event -> Stream IO (Stored event)
getEventStream' pgt =
    fst
        <$> mkEventStream
            (pgt ^. field @"chunkSize")
            (pgt ^. field @"transaction" . field @"connectionResource" . field @"resource")
            (pgt ^. field @"eventTableName" . to mkEventQuery)

-- | A transaction that is always rolled back at the end.
-- This is useful when using cursors as they can only be used inside a transaction.
withStreamReadTransaction
    :: forall m a model event
     . HasCallStack
    => (Stream.MonadAsync m, MonadCatch m)
    => PostgresEvent model event
    -> (PostgresEventTrans model event -> Stream m a)
    -> Stream m a
withStreamReadTransaction pg = Stream.bracket startTrans rollbackTrans
  where
    startTrans :: m (PostgresEventTrans model event)
    startTrans = liftIO $ do
        (connR, localPool) <- takeResource (connectionPool pg)
        t0 <- getCurrentTime
        PG.begin $ Pool.resource connR
        pure $
            PostgresEventTrans
                { transaction = OngoingTransaction connR localPool t0
                , eventTableName = pg ^. field @"eventTableName"
                , modelIORef = pg ^. field @"modelIORef"
                , app = pg ^. field @"app"
                , seed = pg ^. field @"seed"
                , chunkSize = pg ^. field @"chunkSize"
                , logger = pg ^. field @"logger"
                }

    rollbackTrans :: PostgresEventTrans model event -> m ()
    rollbackTrans pgt = liftIO $ do
        -- Nothing changes. We just need the transaction to be able to stream events.
        let OngoingTransaction connR localPool t0 = pgt ^. field' @"transaction"
            conn = Pool.resource connR

            giveBackConn :: IO ()
            giveBackConn = do
                PG.rollback conn
                putResource localPool conn
                t1 <- getCurrentTime
                pgt ^. field' @"logger" $
                    DbTransactionDuration (PrettyCallStack callStack) (diffUTCTime t1 t0)
        giveBackConn `catchAll` \_ -> do
            t1 <- getCurrentTime
            pgt ^. field' @"logger" $
                DbTransactionDuration (PrettyCallStack callStack) (diffUTCTime t1 t0)
            destroyResource (connectionPool pg) localPool conn

withIOTrans
    :: forall a model event
     . HasCallStack
    => PostgresEvent model event
    -> (PostgresEventTrans model event -> IO a)
    -> IO a
withIOTrans pg f = do
    transactionCompleted <- newIORef False
    (connR, localPool) <- do
        t0 <- getCurrentTime
        r <- takeResource (connectionPool pg)
        t1 <- getCurrentTime
        pg ^. field @"logger" $
            WaitForConnectionDuration (PrettyCallStack callStack) (diffUTCTime t1 t0)
        pure r
    bracket (prepareTransaction connR localPool) (cleanup transactionCompleted) $ \pgt -> do
        a <- f pgt
        writeIORef transactionCompleted True
        pure a
  where
    cleanup :: IORef Bool -> PostgresEventTrans model event -> IO ()
    cleanup transactionCompleted pgt = do
        let OngoingTransaction connR localPool t0 = pgt ^. field' @"transaction"
            conn = Pool.resource connR

            giveBackConn :: IO ()
            giveBackConn = do
                readIORef transactionCompleted >>= \case
                    True -> PG.commit conn
                    False -> PG.rollback conn
                Pool.putResource localPool conn
                t1 <- getCurrentTime
                pgt ^. field' @"logger" $
                    DbTransactionDuration (PrettyCallStack callStack) (diffUTCTime t1 t0)
        giveBackConn `catchAll` \_ -> do
            t1 <- getCurrentTime
            pgt ^. field' @"logger" $
                DbTransactionDuration (PrettyCallStack callStack) (diffUTCTime t1 t0)
            destroyResource (connectionPool pg) localPool conn

    prepareTransaction
        :: Pool.Resource Connection
        -> LocalPool Connection
        -> IO (PostgresEventTrans model event)
    prepareTransaction connR localPool = do
        t0 <- getCurrentTime
        PG.begin $ Pool.resource connR
        pure $
            PostgresEventTrans
                { transaction = OngoingTransaction connR localPool t0
                , eventTableName = pg ^. field @"eventTableName"
                , modelIORef = pg ^. field @"modelIORef"
                , app = pg ^. field @"app"
                , seed = pg ^. field @"seed"
                , chunkSize = pg ^. field @"chunkSize"
                , logger = pg ^. field @"logger"
                }

mkEventStream
    :: FromJSON event
    => ChunkSize
    -> Connection
    -> EventQuery
    -> Stream IO (Stored event, EventNumber)
mkEventStream chunkSize conn q = do
    let step :: Cursor.Cursor -> IO (Maybe (Seq EventRowOut, Cursor.Cursor))
        step cursor = do
            r <- Cursor.foldForward cursor chunkSize (\a r -> pure (a :|> r)) Seq.Empty
            case r of
                Left Seq.Empty -> pure Nothing
                Left a -> pure $ Just (a, cursor)
                Right a -> pure $ Just (a, cursor)

    Stream.bracketIO
        (Cursor.declareCursor conn (getPgQuery q))
        (Cursor.closeCursor)
        ( \cursor ->
            Stream.mapM fromEventRow $
                Stream.unfoldMany Unfold.fromList . fmap toList $
                    Stream.unfoldrM
                        step
                        cursor
        )

getModel' :: forall e m. FromJSON e => PostgresEventTrans m e -> IO m
getModel' pgt = do
    NumberedModel model lastEventNo <- readIORef (pgt ^. field @"modelIORef")
    hasNewEvents <-
        queryHasEventsAfter
            (pgt ^. field @"transaction" . field @"connectionResource" . field @"resource")
            (pgt ^. field @"eventTableName")
            lastEventNo
    if hasNewEvents then fst <$> refreshModel pgt else pure model

refreshModel
    :: forall m e
     . FromJSON e
    => PostgresEventTrans m e
    -> IO (m, EventNumber)
refreshModel pgt = withExclusiveLock pgt $ do
    -- refresh doesn't write any events but changes the state and thus needs a lock
    NumberedModel model lastEventNo <- readIORef (pgt ^. field @"modelIORef")
    let eventStream =
            mkEventStream
                (pgt ^. field @"chunkSize")
                (pgt ^. field @"transaction" . field @"connectionResource" . field @"resource")
                (mkEventsAfterQuery (pgt ^. field @"eventTableName") lastEventNo)

        applyModel :: NumberedModel m -> (Stored e, EventNumber) -> NumberedModel m
        applyModel (NumberedModel m _) (ev, evNumber) =
            NumberedModel ((pgt ^. field @"app") m ev) evNumber

    NumberedModel newModel lastNewEventNo <-
        Stream.fold
            ( Fold.foldl'
                applyModel
                (NumberedModel model lastEventNo)
            )
            eventStream

    _ <- writeIORef (pgt ^. field @"modelIORef") $ NumberedModel newModel lastNewEventNo
    pure (newModel, lastNewEventNo)

exclusiveLock :: OngoingTransaction -> EventTableName -> IO ()
exclusiveLock (OngoingTransaction connR _ _) etName =
    void $
        execute_ (Pool.resource connR) ("lock \"" <> fromString etName <> "\" in exclusive mode")

withExclusiveLock :: PostgresEventTrans m e -> IO a -> IO a
withExclusiveLock pgt a = do
    t0 <- getCurrentTime
    exclusiveLock (pgt ^. field' @"transaction") (pgt ^. field @"eventTableName")
    r <- a
    t1 <- getCurrentTime
    pgt ^. field' @"logger" $
        EventTableLockDuration (PrettyCallStack callStack) (diffUTCTime t1 t0)
    pure r

instance (ToJSON e, FromJSON e) => WriteModel (PostgresEvent m e) where
    postUpdateHook pg m e = liftIO $ (pg ^. field @"updateHook") pg m e

    transactionalUpdate pg cmd = withRunInIO $ \runInIO ->
        withIOTrans pg $ \pgt -> withExclusiveLock pgt $ do
            m <- getModel' pgt
            (returnFun, evs) <- runInIO $ cmd m
            NumberedModel m' _ <- readIORef (pg ^. field @"modelIORef")
            storedEvs <- traverse toStored evs
            newNumberedModel <-
                uncurry NumberedModel
                    <$> concurrently
                        ( Stream.fold
                            (Fold.foldl' (pg ^. field @"app") m')
                            (Stream.fromList storedEvs)
                        )
                        ( writeEvents
                            (pgt ^. field @"transaction" . field @"connectionResource" . field @"resource")
                            (pg ^. field @"eventTableName")
                            storedEvs
                        )
            _ <- writeIORef (pg ^. field @"modelIORef") newNumberedModel
            pure $ (model newNumberedModel, storedEvs, returnFun)
