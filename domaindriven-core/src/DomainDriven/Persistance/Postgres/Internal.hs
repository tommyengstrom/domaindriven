-- | Postgres events with state as an IORef
module DomainDriven.Persistance.Postgres.Internal where

import Control.Concurrent (getNumCapabilities)
import Control.Applicative ((<|>))
import Control.DeepSeq (NFData, force)
import Control.Exception (SomeAsyncException, SomeException, evaluate, fromException, throwIO, try)
import Control.Monad
import Control.Monad.Catch (MonadCatch, bracket, finally, throwM, tryJust)
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Foldable
import Data.Generics.Labels ()
import Data.Generics.Product
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable, hash)
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.IORef
import Data.Int
import Data.Pool.Introspection as Pool
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.String
import Data.Text.Encoding qualified as Text
import Data.Time
import Data.UUID (UUID)
import Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.Cursor qualified as Cursor
import DomainDriven.Persistance.Class
import DomainDriven.Persistance.Postgres.Types
import DomainDriven.Persistance.Snapshot
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
import System.Timeout qualified as Timeout
import UnliftIO (MonadUnliftIO (..), concurrently)
import Prelude

-- | Log entries for the persistance layer.
-- Not that OneLineCallStack has contains the CallStack, but prints only the call site.
data LogEntry
    = DbTransactionDuration NominalDiffTime OneLineCallStack
    | EventTableLockDuration NominalDiffTime OneLineCallStack
    | EventTableMigrationDuration NominalDiffTime EventTableName
    | WaitForConnectionDuration NominalDiffTime OneLineCallStack
    | SnapshotOperationFailure String
    deriving (Show, Generic)

newtype OneLineCallStack = OneLineCallStack CallStack

instance Show OneLineCallStack where
    show (OneLineCallStack c) = showOnlyCallSite c

-- | An attempt to create short and informative log messages
showOnlyCallSite :: CallStack -> String
showOnlyCallSite stack = go (getCallStack stack)
  where
    go :: [(String, SrcLoc)] -> String
    go = \case
        [(fun, srcLoc)] ->
            "from "
                <> show fun
                <> " called on line "
                <> show (srcLoc ^. field @"srcLocStartLine")
                <> " in "
                <> show (srcLoc ^. field @"srcLocFile")
        _ : xs -> go xs
        [] -> ""

data PostgresEvent index model event = PostgresEvent
    { connectionPool :: Pool Connection
    , eventTableName :: EventTableName
    , modelIORef :: IORef (HashMap index (NumberedModel model))
    , app :: model -> Stored event -> model
    , seed :: model
    , chunkSize :: ChunkSize
    -- ^ Number of events fetched from Postgres per cursor batch. A round-trip /
    -- memory knob, independent of parse parallelism.
    , parseConcurrency :: ParseConcurrency
    -- ^ Number of parser threads that parse a fetched batch concurrently.
    , updateHook
        :: PostgresEvent index model event
        -> index
        -> model
        -> [Stored event]
        -> IO ()
    , logger :: LogEntry -> IO ()
    , snapshotConfig :: Maybe (SnapshotConfig model)
    , snapshotWritesInFlight :: IORef (HashSet index)
    }
    deriving (Generic)

data PostgresEventTrans index model event = PostgresEventTrans
    { transaction :: OngoingTransaction
    , eventTableName :: EventTableName
    , modelIORef :: IORef (HashMap index (NumberedModel model))
    , app :: model -> Stored event -> model
    , seed :: model
    , chunkSize :: ChunkSize
    -- ^ Number of events fetched from Postgres per cursor batch. A round-trip /
    -- memory knob, independent of parse parallelism.
    , parseConcurrency :: ParseConcurrency
    -- ^ Number of parser threads that parse a fetched batch concurrently.
    , logger :: LogEntry -> IO ()
    }
    deriving (Generic)

instance (IsPgIndex i, FromJSON e, NFData e) => ReadModel (PostgresEvent i m e) where
    type Model (PostgresEvent i m e) = m
    type Index (PostgresEvent i m e) = i
    type Event (PostgresEvent i m e) = e
    applyEvent pg = pg ^. field @"app"
    getModel pg index = liftIO $ getModelIO pg index

    getEventList pg index = withResource (connectionPool pg) $ \conn ->
        fmap fst
            <$> queryEventsWithParseConcurrency
                (pg ^. field @"parseConcurrency")
                (pg ^. field @"chunkSize")
                (Pool.resource conn)
                (pg ^. field @"eventTableName")
                index

    getEventStream pg = withStreamReadTransaction pg . flip getEventStream'

getEventTableName :: EventTable -> EventTableName
getEventTableName = validate . go 0
  where
    go :: Int -> EventTable -> String
    go i = \case
        MigrateUsing _ u -> go (i + 1) u
        InitialVersion n -> n <> "_v" <> show (i + 1)
    validate name
        | all isValidChar name && not (null name) = name
        | otherwise =
            error $
                "[DomainDriven] Invalid event table name: "
                    <> show name
                    <> ". Names must be non-empty and contain only [a-zA-Z0-9_]."
    isValidChar c = isAsciiLower c || isAsciiUpper c || isDigit c || c == '_'

-- | Create the table required for storing state and events, if they do not yet exist.
createEventTable :: PostgresEventTrans index model event -> IO ()
createEventTable pgt = do
    void $
        createEventTable'
            (pgt ^. #transaction . #connectionResource . #resource)
            (pgt ^. #eventTableName)

createEventTable' :: Connection -> EventTableName -> IO Int64
createEventTable' conn eventTable = do
    _ <-
        execute_ conn $
            "create table if not exists "
                <> quoteIdent eventTable
                <> " \
                   \( id uuid primary key\
                   \, index varchar not null\
                   \, event_number bigint not null generated always as identity\
                   \, timestamp timestamptz not null default now()\
                   \, event jsonb not null\
                   \);"
    execute_ conn $
        "create index on "
            <> quoteIdent eventTable
            <> " (index, event_number);"

retireTable :: Connection -> EventTableName -> IO ()
retireTable conn tableName = do
    createRetireFunction conn
    void $
        execute_ conn $
            "create trigger retired before insert on "
                <> quoteIdent tableName
                <> " execute procedure retired_table()"

createRetireFunction :: Connection -> IO ()
createRetireFunction conn =
    void
        . execute_ conn
        $ "create or replace function retired_table() returns trigger as \
          \$$ begin raise exception 'Event table has been retired.'; end; $$ \
          \language plpgsql;"

-- | Create a connection pool with default settings (1 stripe, 5 connections, 60s idle).
simplePool :: MonadUnliftIO m => IO Connection -> m (Pool Connection)
simplePool = simplePoolWith id

-- | Create a connection pool, applying a modifier to the default PoolConfig.
simplePoolWith
    :: MonadUnliftIO m
    => (Pool.PoolConfig Connection -> Pool.PoolConfig Connection)
    -> IO Connection
    -> m (Pool Connection)
simplePoolWith modifyConfig getConn = do
    -- Using a single stripe to ensures all thread can use all connections
    let poolCfg =
            modifyConfig
                . Pool.setNumStripes (Just 1)
                $ Pool.defaultPoolConfig (liftIO getConn) (liftIO . PG.close) 60 5
    liftIO $ Pool.newPool poolCfg

simplePool' :: MonadUnliftIO m => PG.ConnectInfo -> m (Pool Connection)
simplePool' = simplePoolWith' id

simplePoolWith'
    :: MonadUnliftIO m
    => (Pool.PoolConfig Connection -> Pool.PoolConfig Connection)
    -> PG.ConnectInfo
    -> m (Pool Connection)
simplePoolWith' modifyConfig connInfo = simplePoolWith modifyConfig (PG.connect connInfo)

-- | Setup the persistance model and verify that the tables exist.
postgresWriteModelNoMigration
    :: HasCallStack
    => Pool Connection
    -> EventTableName
    -> (model -> Stored event -> model)
    -> model
    -> IO (PostgresEvent index model event)
postgresWriteModelNoMigration pool eventTable app' seed' = do
    pg <- createPostgresPersistance pool eventTable app' seed'
    withIOTrans pg createEventTable
    pure pg

postgresWriteModelNoMigrationWithSnapshots
    :: forall index model event
     . HasCallStack
    => Pool Connection
    -> EventTableName
    -> SnapshotConfig model
    -> (model -> Stored event -> model)
    -> model
    -> IO (PostgresEvent index model event)
postgresWriteModelNoMigrationWithSnapshots pool eventTable snapshots app' seed' = do
    initializeSnapshotStore (snapshotStore snapshots)
    pg <- createPostgresPersistance pool eventTable app' seed'
    withIOTrans pg createEventTable
    pure (pg :: PostgresEvent index model event){snapshotConfig = Just snapshots}

-- | Setup the persistance model and verify that the tables exist.
postgresWriteModel
    :: HasCallStack
    => Pool Connection
    -> EventTable
    -> (model -> Stored event -> model)
    -> model
    -> IO (PostgresEvent index model event)
postgresWriteModel pool eventTable app' seed' = do
    pg <- createPostgresPersistance pool (getEventTableName eventTable) app' seed'
    withIOTrans pg $ \pgt -> runMigrations (pgt ^. field @"logger") (pgt ^. field @"transaction") eventTable
    pure pg

postgresWriteModelWithSnapshots
    :: forall index model event
     . HasCallStack
    => Pool Connection
    -> EventTable
    -> SnapshotConfig model
    -> (model -> Stored event -> model)
    -> model
    -> IO (PostgresEvent index model event)
postgresWriteModelWithSnapshots pool eventTable snapshots app' seed' = do
    initializeSnapshotStore (snapshotStore snapshots)
    pg <- createPostgresPersistance pool (getEventTableName eventTable) app' seed'
    withIOTrans pg $ \pgt -> runMigrations (pgt ^. field @"logger") (pgt ^. field @"transaction") eventTable
    pure (pg :: PostgresEvent index model event){snapshotConfig = Just snapshots}

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
            exclusiveLock trans (getEventTableName prevEt) NoIndex
            t0 <- getCurrentTime
            createTable
            mig (getEventTableName prevEt) (getEventTableName et) conn
            retireTable conn (getEventTableName prevEt)
            t1 <- getCurrentTime
            logger $ EventTableMigrationDuration (diffUTCTime t1 t0) (getEventTableName et)
        (_, r) -> fail $ "Unexpected table query result: " <> show r
  where
    conn :: Connection
    conn = trans ^. field @"connectionResource" . field @"resource"

    createTable :: IO ()
    createTable = do
        let tableName = getEventTableName et
        void $ createEventTable' conn tableName

createPostgresPersistance
    :: forall event index model
     . Pool Connection
    -> EventTableName
    -> (model -> Stored event -> model)
    -- ^ Apply event
    -> model
    -- ^ Initial model
    -> IO (PostgresEvent index model event)
createPostgresPersistance pool eventTable app' seed' = do
    ref <- newIORef HM.empty
    snapshotWrites <- newIORef HS.empty
    defaultParseConcurrency <- max 1 <$> getNumCapabilities
    pure $
        PostgresEvent
            { connectionPool = pool
            , eventTableName = eventTable
            , modelIORef = ref
            , app = app'
            , seed = seed'
            , chunkSize = defaultReadChunkSize
            , parseConcurrency = defaultParseConcurrency
            , updateHook = \_ _ _ _ -> pure ()
            , logger = \case
                e@(DbTransactionDuration dt _) -> when (dt > 1) $ putStrLn $ "[DomainDriven] " <> show e
                e@(EventTableLockDuration dt _) -> when (dt > 0.5) $ putStrLn $ "[DomainDriven] " <> show e
                EventTableMigrationDuration dt etName -> putStrLn $ "[DomainDriven] migration of " <> etName <> " completed in " <> show dt
                e@(WaitForConnectionDuration dt _) -> when (dt > 0.5) $ putStrLn $ "[DomainDriven] " <> show e
                SnapshotOperationFailure failure -> putStrLn $ "[DomainDriven] snapshot operation failed: " <> failure
            , snapshotConfig = Nothing
            , snapshotWritesInFlight = snapshotWrites
            }

-- | Default number of events fetched per Postgres cursor batch. Also sets the
-- parse-task granularity: each batch is split into @chunkSize \`div\`
-- parseConcurrency@-row tasks across the parser threads.
defaultReadChunkSize :: ChunkSize
defaultReadChunkSize = 2048

queryEvents
    :: forall a index
     . (IsPgIndex index, FromJSON a, NFData a)
    => Connection
    -> EventTableName
    -> index
    -> IO [(Stored a, EventNumber)]
queryEvents = queryEventsWithParseConcurrency 1 defaultReadChunkSize

queryEventsWithParseConcurrency
    :: forall a index
     . (IsPgIndex index, FromJSON a, NFData a)
    => ParseConcurrency
    -> ChunkSize
    -> Connection
    -> EventTableName
    -> index
    -> IO [(Stored a, EventNumber)]
queryEventsWithParseConcurrency workers chunkSize conn eventTable index = do
    parseEventRows workers chunkSize =<< query conn q (Only $ toPgIndex index)
  where
    q :: PG.Query
    q =
        "select id, event_number,timestamp,event::text from "
            <> quoteIdent eventTable
            <> " where index = ? order by event_number"

queryEventsAfter
    :: (FromJSON a, NFData a)
    => Connection
    -> EventTableName
    -> EventNumber
    -> IO [(Stored a, EventNumber)]
queryEventsAfter = queryEventsAfterWithParseConcurrency 1 defaultReadChunkSize

queryEventsAfterWithParseConcurrency
    :: (FromJSON a, NFData a)
    => ParseConcurrency
    -> ChunkSize
    -> Connection
    -> EventTableName
    -> EventNumber
    -> IO [(Stored a, EventNumber)]
queryEventsAfterWithParseConcurrency workers chunkSize conn eventTable (EventNumber lastEvent) =
    parseEventRows workers chunkSize
        =<< query
            conn
            ( "select id, event_number,timestamp,event::text from "
                <> quoteIdent eventTable
                <> " where event_number > ? order by event_number"
            )
            (Only lastEvent)

newtype EventQuery = EventQuery {getPgQuery :: Connection -> IO PG.Query}

mkEventsAfterQuery
    :: IsPgIndex index
    => EventTableName
    -> index
    -> EventNumber
    -> EventQuery
mkEventsAfterQuery eventTable index (EventNumber lastEvent) =
    EventQuery $ \conn -> do
        formatted <-
            formatQuery
                conn
                ( "select id, event_number,timestamp,event::text from "
                    <> quoteIdent eventTable
                    <> " where index = ? and event_number > ? order by event_number"
                )
                (toPgIndex index, lastEvent)
        pure . fromString $ BS8.unpack formatted

mkEventQuery :: IsPgIndex index => EventTableName -> index -> EventQuery
mkEventQuery eventTable index =
    EventQuery $ \conn -> do
        formatted <-
            formatQuery
                conn
                ( "select id, event_number,timestamp,event::text from "
                    <> quoteIdent eventTable
                    <> " where index = ? order by event_number"
                )
                (Only $ toPgIndex index)
        pure . fromString $ BS8.unpack formatted

headMay :: [a] -> Maybe a
headMay = \case
    a : _ -> Just a
    [] -> Nothing

queryHasEventsAfter :: IsPgIndex index => Connection -> EventTableName -> index -> EventNumber -> IO Bool
queryHasEventsAfter conn eventTable index (EventNumber lastEvent) =
    maybe True fromOnly . headMay <$> query conn q (toPgIndex index, lastEvent)
  where
    q :: PG.Query
    q =
        "select exists(select 1 from "
            <> quoteIdent eventTable
            <> " where index = ? and event_number > ?)"

-- writeEvents
--     :: forall a
--      . ToJSON a
--     => Connection
--     -> EventTableName
--     -> [Stored a]
--     -> IO EventNumber
-- writeEvents conn eventTable storedEvents = do
--     _ <-
--         executeMany
--             conn
--             ( "insert into \""
--                 <> fromString eventTable
--                 <> "\" (id, timestamp, event) \
--                    \values (?, ?, ?)"
--             )
--             ( fmap
--                 (\x -> (storedUUID x, storedTimestamp x, encode $ storedEvent x))
--                 storedEvents
--             )
--     foldl' max 0 . fmap fromOnly
--         <$> query_
--             conn
--             ("select coalesce(max(event_number),1) from \"" <> fromString eventTable <> "\"")
writeEvents
    :: forall a index
     . ( ToJSON a
       , IsPgIndex index
       )
    => Connection
    -> EventTableName
    -> index
    -> [Stored a]
    -> IO EventNumber
writeEvents conn eventTable index storedEvents =
    maybe 0 (\EventCheckpoint{eventNumber = number} -> number)
        <$> writeEventsCheckpoint conn eventTable index storedEvents

writeEventsCheckpoint
    :: forall a index
     . (ToJSON a, IsPgIndex index)
    => Connection
    -> EventTableName
    -> index
    -> [Stored a]
    -> IO (Maybe EventCheckpoint)
writeEventsCheckpoint conn eventTable index storedEvents =
    case storedEvents of
        [] -> pure Nothing
        _ -> do
            rows <-
                query
                    conn
                    ( "with inserted as (insert into "
                        <> quoteIdent eventTable
                        <> " (id, index, timestamp, event) select id, index, timestamp, event from jsonb_to_recordset(?::jsonb) as batch(id uuid, index varchar, timestamp timestamptz, event jsonb) returning event_number, id) select event_number, id from inserted order by event_number"
                    )
                    (Only $ Text.decodeUtf8 $ LBS.toStrict $ encode $ fmap eventRow storedEvents)
                    :: IO [(EventNumber, UUID)]
            pure $ foldl' (\_ (number, eventId) -> Just $ EventCheckpoint number eventId) Nothing rows
  where
    eventRow :: Stored a -> Value
    eventRow stored =
        object
            [ "id" .= storedUUID stored
            , "index" .= toPgIndex index
            , "timestamp" .= storedTimestamp stored
            , "event" .= storedEvent stored
            ]

getEventStream'
    :: ( FromJSON event
       , NFData event
       , IsPgIndex index
       )
    => PostgresEventTrans index model event
    -> index
    -> Stream IO (Stored event)
getEventStream' pgt index =
    fst
        <$> mkEventStreamWithParseConcurrency
            (pgt ^. #parseConcurrency)
            (pgt ^. #chunkSize)
            (pgt ^. #transaction . #connectionResource . #resource)
            (pgt ^. #eventTableName . to (`mkEventQuery` index))

-- | A transaction that is always rolled back at the end.
-- This is useful when using cursors as they can only be used inside a transaction.
withStreamReadTransaction
    :: forall m a index model event
     . HasCallStack
    => (Stream.MonadAsync m, MonadCatch m)
    => PostgresEvent index model event
    -> (PostgresEventTrans index model event -> Stream m a)
    -> Stream m a
withStreamReadTransaction pg = Stream.bracket startTrans rollbackTrans
  where
    startTrans :: m (PostgresEventTrans index model event)
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
                , parseConcurrency = pg ^. field @"parseConcurrency"
                , logger = pg ^. field @"logger"
                }

    rollbackTrans :: PostgresEventTrans index model event -> m ()
    rollbackTrans pgt = liftIO $ do
        -- Nothing changes. We just need the transaction to be able to stream events.
        let OngoingTransaction connR localPool t0 = pgt ^. field' @"transaction"
            conn = Pool.resource connR
        rollbackResult <- try @SomeException $ PG.rollback conn
        case rollbackResult of
            Left _ -> destroyResource (connectionPool pg) localPool conn
            Right () -> Pool.putResource localPool conn
        t1 <- getCurrentTime
        logIgnoringFailures
            (pgt ^. field' @"logger")
            (DbTransactionDuration (diffUTCTime t1 t0) (OneLineCallStack callStack))

withIOTrans
    :: forall a index model event
     . HasCallStack
    => PostgresEvent index model event
    -> (PostgresEventTrans index model event -> IO a)
    -> IO a
withIOTrans pg f = do
    transactionCompleted <- newIORef False
    (connR, localPool) <- do
        t0 <- getCurrentTime
        r <- takeResource (connectionPool pg)
        t1 <- getCurrentTime
        logIgnoringFailures
            (pg ^. field @"logger")
            (WaitForConnectionDuration (diffUTCTime t1 t0) (OneLineCallStack callStack))
        pure r
    bracket (prepareTransaction connR localPool) (cleanup transactionCompleted) $ \pgt -> do
        a <- f pgt
        writeIORef transactionCompleted True
        pure a
  where
    cleanup :: IORef Bool -> PostgresEventTrans index model event -> IO ()
    cleanup transactionCompleted pgt = do
        let OngoingTransaction connR localPool t0 = pgt ^. field' @"transaction"
            conn = Pool.resource connR
        completed <- readIORef transactionCompleted
        transactionResult <- try @SomeException $ case completed of
            True -> PG.commit conn
            False -> PG.rollback conn
        case transactionResult of
            Left failure -> do
                destroyResource (connectionPool pg) localPool conn
                logTransactionDuration pgt t0
                throwIO failure
            Right () -> do
                returnResult <- try @SomeException $ Pool.putResource localPool conn
                case returnResult of
                    Left failure -> do
                        destroyResource (connectionPool pg) localPool conn
                        logTransactionDuration pgt t0
                        throwIO failure
                    Right () -> logTransactionDuration pgt t0

    logTransactionDuration :: PostgresEventTrans index model event -> UTCTime -> IO ()
    logTransactionDuration pgt t0 = do
        t1 <- getCurrentTime
        logIgnoringFailures
            (pgt ^. field' @"logger")
            (DbTransactionDuration (diffUTCTime t1 t0) (OneLineCallStack callStack))

    prepareTransaction
        :: Pool.Resource Connection
        -> LocalPool Connection
        -> IO (PostgresEventTrans index model event)
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
                , parseConcurrency = pg ^. field @"parseConcurrency"
                , logger = pg ^. field @"logger"
                }

mkEventStream
    :: (FromJSON event, NFData event)
    => ChunkSize
    -> Connection
    -> EventQuery
    -> Stream IO (Stored event, EventNumber)
mkEventStream = mkEventStreamWithParseConcurrency 1

mkEventStreamWithParseConcurrency
    :: (FromJSON event, NFData event)
    => ParseConcurrency
    -> ChunkSize
    -> Connection
    -> EventQuery
    -> Stream IO (Stored event, EventNumber)
mkEventStreamWithParseConcurrency parseConcurrency chunkSize conn q = do
    let step :: Cursor.Cursor -> IO (Maybe (Seq EventRowOut, Cursor.Cursor))
        step cursor = do
            r <- Cursor.foldForward cursor chunkSize (\a r -> pure (a :|> r)) Seq.Empty
            case r of
                Left Seq.Empty -> pure Nothing
                Left a -> pure $ Just (a, cursor)
                Right a -> pure $ Just (a, cursor)

    Stream.bracketIO
        (getPgQuery q conn >>= Cursor.declareCursor conn)
        Cursor.closeCursor
        ( Stream.unfoldEach Unfold.fromList
            . Stream.mapM (parseEventRows parseConcurrency chunkSize . toList)
            . Stream.unfoldrM step
        )

-- | Parse and fully force a single fetched event row. Run on a parser thread so
-- the (deep) parse cost stays off the consuming thread.
parseRowResult
    :: (FromJSON event, NFData event)
    => EventRowOut
    -> IO (Either PersistanceError (Stored event, EventNumber))
parseRowResult = evaluate . force . fromEventRowResult

-- | Parse a batch of fetched event rows, fully forcing each parsed event off the
-- calling thread. Up to @workers@ parser threads parse concurrently; results are
-- returned in input order, and the first parse error (by input order) is thrown.
--
-- Rows are split into @chunkSize \`div\` workers@-row tasks: roughly one task per
-- worker per fetched batch, so the parse granularity scales inversely with the
-- worker count (more cores → more, finer tasks for better balance) and needs no
-- separate tuning knob. Together with 'Stream.eager' this matches a hand-rolled
-- thread pool at low core counts and beats it at high core counts.
parseEventRows
    :: (FromJSON event, NFData event)
    => ParseConcurrency
    -> ChunkSize
    -> [EventRowOut]
    -> IO [(Stored event, EventNumber)]
parseEventRows workers chunkSize rows = do
    let taskSize = max 1 (chunkSize `div` max 1 workers)
    parsed <-
        if workers <= 1
            then traverse parseRowResult rows
            else
                Stream.fold Fold.toList
                    . Stream.unfoldEach Unfold.fromList
                    . Stream.parMapM
                        ( Stream.maxThreads workers
                            . Stream.eager True
                            . Stream.ordered True
                        )
                        (traverse parseRowResult)
                    . Stream.foldMany (Fold.take taskSize Fold.toList)
                    $ Stream.fromList rows
    either throwM pure (sequence parsed)

data LoadedSnapshot model = LoadedSnapshot
    { loadedStoredSnapshot :: !StoredSnapshot
    , loadedModel :: !(Either SnapshotFailure model)
    }

getModelIO
    :: forall e index m
     . (HasCallStack, IsPgIndex index, FromJSON e, NFData e)
    => PostgresEvent index m e
    -> index
    -> IO m
getModelIO pg index = do
    cached <- HM.lookup index <$> readIORef (pg ^. field @"modelIORef")
    loaded <- case cached of
        Just _ -> pure Nothing
        Nothing -> loadSnapshotCandidate pg index
    (state, invalidSnapshot) <- withIOTrans pg $ \pgt -> do
        current <- HM.lookup index <$> readIORef (pgt ^. field @"modelIORef")
        case current of
            Just currentState -> do
                hasNewEvents <- queryHasEventsAfter (transactionConnection pgt) (pgt ^. field @"eventTableName") index (stateEventNumber currentState)
                if hasNewEvents
                    then withExclusiveLock pgt index $ reconstructModel pgt index loaded
                    else pure (currentState, Nothing)
            Nothing -> withExclusiveLock pgt index $ reconstructModel pgt index loaded
    published <- publishModel pg index state
    traverse_ (deleteInvalidSnapshot pg index) invalidSnapshot
    attemptSnapshot pg index published
    pure (model published)

reconstructModel
    :: forall i m e
     . (IsPgIndex i, FromJSON e, NFData e)
    => PostgresEventTrans i m e
    -> i
    -> Maybe (LoadedSnapshot m)
    -> IO (NumberedModel m, Maybe StoredSnapshot)
reconstructModel pgt index loaded = do
    cached <- HM.lookup index <$> readIORef (pgt ^. field @"modelIORef")
    (initialState, invalidSnapshot) <- case cached of
        Just state -> pure (state, Nothing)
        Nothing -> stateFromSnapshot pgt index loaded
    state <- replayEvents pgt index initialState
    pure (state, invalidSnapshot)

stateFromSnapshot
    :: IsPgIndex i
    => PostgresEventTrans i m e
    -> i
    -> Maybe (LoadedSnapshot m)
    -> IO (NumberedModel m, Maybe StoredSnapshot)
stateFromSnapshot pgt index = \case
    Nothing -> pure (seedState pgt, Nothing)
    Just LoadedSnapshot{loadedStoredSnapshot, loadedModel} -> case loadedModel of
        Left _ -> pure (seedState pgt, Just loadedStoredSnapshot)
        Right snapshotModel -> do
            valid <- validateSnapshotCheckpoint pgt index (storedSnapshotCheckpoint loadedStoredSnapshot)
            pure $
                if valid
                    then (NumberedModel snapshotModel (Just $ fromSnapshotCheckpoint $ storedSnapshotCheckpoint loadedStoredSnapshot) 0, Nothing)
                    else (seedState pgt, Just loadedStoredSnapshot)

seedState :: HasField' "seed" pg m => pg -> NumberedModel m
seedState pg = NumberedModel (pg ^. field' @"seed") Nothing 0

replayEvents
    :: forall i m e
     . (IsPgIndex i, FromJSON e, NFData e)
    => PostgresEventTrans i m e
    -> i
    -> NumberedModel m
    -> IO (NumberedModel m)
replayEvents pgt index initialState =
    Stream.fold (Fold.foldl' applyModel initialState) eventStream
  where
    eventStream :: Stream IO (Stored e, EventNumber)
    eventStream =
        mkEventStreamWithParseConcurrency
            (pgt ^. field @"parseConcurrency")
            (pgt ^. field @"chunkSize")
            (transactionConnection pgt)
            (mkEventsAfterQuery (pgt ^. field @"eventTableName") index (stateEventNumber initialState))

    applyModel :: NumberedModel m -> (Stored e, EventNumber) -> NumberedModel m
    applyModel NumberedModel{model = currentModel, eventsSinceSnapshot} (stored, number) =
        NumberedModel
            ((pgt ^. field @"app") currentModel stored)
            (Just $ EventCheckpoint number (storedUUID stored))
            (eventsSinceSnapshot + 1)

validateSnapshotCheckpoint
    :: IsPgIndex i
    => PostgresEventTrans i m e
    -> i
    -> SnapshotCheckpoint
    -> IO Bool
validateSnapshotCheckpoint pgt index SnapshotCheckpoint{snapshotEventNumber, snapshotEventId} = do
    rows <-
        query
            (transactionConnection pgt)
            ( "select exists(select 1 from "
                <> quoteIdent (pgt ^. field @"eventTableName")
                <> " where index = ? and event_number = ? and id = ?)"
            )
            (toPgIndex index, snapshotEventNumber, snapshotEventId)
            :: IO [Only Bool]
    pure $ case rows of
        [Only valid] -> valid
        _ -> False

transactionConnection :: PostgresEventTrans i m e -> Connection
transactionConnection pgt =
    pgt ^. field @"transaction" . field @"connectionResource" . field @"resource"

stateEventNumber :: NumberedModel m -> EventNumber
stateEventNumber NumberedModel{checkpoint} =
    maybe 0 (\EventCheckpoint{eventNumber = number} -> number) checkpoint

fromSnapshotCheckpoint :: SnapshotCheckpoint -> EventCheckpoint
fromSnapshotCheckpoint SnapshotCheckpoint{snapshotEventNumber, snapshotEventId} =
    EventCheckpoint (EventNumber snapshotEventNumber) snapshotEventId

toSnapshotCheckpoint :: EventCheckpoint -> SnapshotCheckpoint
toSnapshotCheckpoint EventCheckpoint{eventNumber = EventNumber number, eventId} =
    SnapshotCheckpoint number eventId

publishModel
    :: Hashable index
    => PostgresEvent index m e
    -> index
    -> NumberedModel m
    -> IO (NumberedModel m)
publishModel pg index candidate =
    atomicModifyIORef' (pg ^. field @"modelIORef") $ \models ->
        let published = case HM.lookup index models of
                Just current | stateEventNumber current > stateEventNumber candidate -> current
                Just current
                    | stateEventNumber current == stateEventNumber candidate
                    , eventsSinceSnapshot current < eventsSinceSnapshot candidate -> current
                Just _ -> candidate
                Nothing -> candidate
         in (HM.insert index published models, published)

loadSnapshotCandidate
    :: IsPgIndex index
    => PostgresEvent index m e
    -> index
    -> IO (Maybe (LoadedSnapshot m))
loadSnapshotCandidate pg index = case pg ^. field @"snapshotConfig" of
    Nothing -> pure Nothing
    Just config -> do
        loaded <- runSnapshotOperation pg config ("load/decode " <> show (snapshotKey pg index)) $ do
            snapshot <- loadSnapshot (snapshotStore config) (snapshotKey pg index)
            traverse (decodeLoaded config) snapshot
        pure $ join loaded
  where
    decodeLoaded :: SnapshotConfig m -> StoredSnapshot -> IO (LoadedSnapshot m)
    decodeLoaded config stored = do
        decoded <-
            if storedSnapshotFormat stored == snapshotCodecFormat (snapshotCodec config)
                then decodeSnapshot (snapshotCodec config) (storedSnapshotPayload stored)
                else pure . Left $ SnapshotFailure "Snapshot format does not match the configured codec"
        pure $ LoadedSnapshot stored decoded

deleteInvalidSnapshot
    :: IsPgIndex index
    => PostgresEvent index m e
    -> index
    -> StoredSnapshot
    -> IO ()
deleteInvalidSnapshot pg index snapshot = case pg ^. field @"snapshotConfig" of
    Nothing -> pure ()
    Just config ->
        void . runSnapshotOperation pg config ("delete " <> show (snapshotKey pg index)) $
            deleteSnapshot (snapshotStore config) (snapshotKey pg index) snapshot

attemptSnapshot
    :: IsPgIndex index
    => PostgresEvent index m e
    -> index
    -> NumberedModel m
    -> IO ()
attemptSnapshot pg index state = case (pg ^. field @"snapshotConfig", checkpoint state) of
    (Just config, Just eventCheckpoint)
        | eventsSinceSnapshot state >= everyNEvents (snapshotFrequency config) -> do
            claimed <- claimSnapshotWrite pg index
            when claimed $
                ( do
                    result <- runSnapshotOperation pg config ("encode/store " <> show (snapshotKey pg index)) $ do
                        encoded <- encodeSnapshot (snapshotCodec config) (model state)
                        bytes <- case encoded of
                            Left failure -> fail (snapshotFailureMessage failure)
                            Right bytes -> pure bytes
                        storeSnapshot
                            (snapshotStore config)
                            (snapshotKey pg index)
                            (toSnapshotCheckpoint eventCheckpoint)
                            (snapshotCodecFormat $ snapshotCodec config)
                            bytes
                    case result of
                        Just SnapshotStored -> markSnapshotStored pg index state
                        Just NewerSnapshotRetained -> pure ()
                        Nothing -> pure ()
                )
                    `finally` releaseSnapshotWrite pg index
    (Just _, Just _) -> pure ()
    (Just _, Nothing) -> pure ()
    (Nothing, Just _) -> pure ()
    (Nothing, Nothing) -> pure ()

claimSnapshotWrite :: IsPgIndex index => PostgresEvent index m e -> index -> IO Bool
claimSnapshotWrite pg index =
    atomicModifyIORef' (pg ^. field @"snapshotWritesInFlight") $ \inFlight ->
        if HS.member index inFlight
            then (inFlight, False)
            else (HS.insert index inFlight, True)

releaseSnapshotWrite :: IsPgIndex index => PostgresEvent index m e -> index -> IO ()
releaseSnapshotWrite pg index =
    atomicModifyIORef' (pg ^. field @"snapshotWritesInFlight") $ \inFlight ->
        (HS.delete index inFlight, ())

markSnapshotStored
    :: Hashable index
    => PostgresEvent index m e
    -> index
    -> NumberedModel m
    -> IO ()
markSnapshotStored pg index storedState =
    atomicModifyIORef' (pg ^. field @"modelIORef") $ \models ->
        let updated = HM.adjust resetIfSame index models
         in (updated, ())
  where
    resetIfSame :: NumberedModel m -> NumberedModel m
    resetIfSame current
        | stateEventNumber current < stateEventNumber storedState = current
        | eventsSinceSnapshot current < eventsSinceSnapshot storedState = current
        | otherwise =
            current
                { eventsSinceSnapshot =
                    eventsSinceSnapshot current - eventsSinceSnapshot storedState
                }

snapshotKey :: IsPgIndex index => PostgresEvent index m e -> index -> SnapshotKey
snapshotKey pg index = SnapshotKey (pg ^. field @"eventTableName") (toPgIndex index)

runSnapshotOperation
    :: PostgresEvent index m e
    -> SnapshotConfig m
    -> String
    -> IO a
    -> IO (Maybe a)
runSnapshotOperation pg config context operation = do
    result <- trySynchronous $ Timeout.timeout timeoutMicros operation
    case result of
        Left failure -> do
            logSnapshotFailure pg (context <> ": " <> show failure)
            pure Nothing
        Right Nothing -> do
            logSnapshotFailure pg (context <> ": operation timed out")
            pure Nothing
        Right (Just value) -> pure (Just value)
  where
    timeoutMicros :: Int
    timeoutMicros = ceiling (realToFrac (snapshotTimeoutDuration $ snapshotTimeout config) * (1000000 :: Double))

logSnapshotFailure :: PostgresEvent index m e -> String -> IO ()
logSnapshotFailure pg failure =
    logIgnoringFailures (pg ^. field @"logger") (SnapshotOperationFailure failure)

logIgnoringFailures :: (LogEntry -> IO ()) -> LogEntry -> IO ()
logIgnoringFailures logger entry = void . trySynchronous $ logger entry

trySynchronous :: IO a -> IO (Either SomeException a)
trySynchronous = tryJust $ \failure -> case fromException failure :: Maybe SomeAsyncException of
    Just _ -> Nothing
    Nothing -> Just failure

exclusiveLock :: IsPgIndex i => OngoingTransaction -> EventTableName -> i -> IO ()
exclusiveLock (OngoingTransaction connR _ _) etName index = do
    -- We use advisory locks in favor of row level locks as we would not have the ability
    -- to lock an index before the first event is written with row level locks.
    void $
        ( query
            (Pool.resource connR)
            "SELECT pg_advisory_xact_lock(?)"
            (Only (fromIntegral (hash (etName, index)) :: Int64))
            :: IO [Only ()]
        )

withExclusiveLock
    :: (HasCallStack, IsPgIndex i) => PostgresEventTrans i m e -> i -> IO a -> IO a
withExclusiveLock pgt index a = do
    exclusiveLock (pgt ^. field' @"transaction") (pgt ^. field @"eventTableName") index
    t0 <- getCurrentTime
    r <- a
    t1 <- getCurrentTime
    pgt ^. field' @"logger" $
        EventTableLockDuration (diffUTCTime t1 t0) (OneLineCallStack callStack)
    pure r

instance (IsPgIndex i, ToJSON e, FromJSON e, NFData e) => WriteModel (PostgresEvent i m e) where
    postUpdateHook pg i m e = liftIO $ (pg ^. field @"updateHook") pg i m e

    transactionalUpdate pg index cmd = withRunInIO $ \runInIO -> do
        cached <- HM.lookup index <$> readIORef (pg ^. field @"modelIORef")
        loaded <- case cached of
            Just _ -> pure Nothing
            Nothing -> loadSnapshotCandidate pg index
        (newState, storedEvents, returnFun, invalidSnapshot) <-
            withIOTrans pg $ \pgt -> withExclusiveLock pgt index $ do
                (currentState, invalid) <- reconstructModel pgt index loaded
                (extractResult, events) <- runInIO $ cmd (model currentState)
                stored <- traverse toStored events
                (newModel, writtenCheckpoint) <-
                    concurrently
                        ( Stream.fold
                            (Fold.foldl' (pg ^. field @"app") (model currentState))
                            (Stream.fromList stored)
                        )
                        ( writeEventsCheckpoint
                            (transactionConnection pgt)
                            (pg ^. field @"eventTableName")
                            index
                            stored
                        )
                let newState =
                        NumberedModel
                            newModel
                            (writtenCheckpoint <|> checkpoint currentState)
                            (eventsSinceSnapshot currentState + fromIntegral (length stored))
                pure (newState, stored, extractResult, invalid)
        _ <- publishModel pg index newState
        traverse_ (deleteInvalidSnapshot pg index) invalidSnapshot
        attemptSnapshot pg index newState
        pure (model newState, storedEvents, returnFun)
