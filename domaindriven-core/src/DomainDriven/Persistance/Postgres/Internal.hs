-- | Postgres events with state as an IORef
module DomainDriven.Persistance.Postgres.Internal where

import Control.Concurrent (getNumCapabilities)
import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Foldable
import Data.Generics.Labels ()
import Data.Generics.Product
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (hash)
import Data.IORef
import Data.Int
import Data.Maybe (fromMaybe)
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

-- | Log entries for the persistance layer.
-- Not that OneLineCallStack has contains the CallStack, but prints only the call site.
data LogEntry
    = DbTransactionDuration NominalDiffTime OneLineCallStack
    | EventTableLockDuration NominalDiffTime OneLineCallStack
    | EventTableMigrationDuration NominalDiffTime EventTableName
    | WaitForConnectionDuration NominalDiffTime OneLineCallStack
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
    getModel pg index = liftIO $ withIOTrans pg (`getModel'` index)

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
    parseEventRows workers chunkSize =<< query_ conn q
  where
    q :: PG.Query
    q =
        "select id, event_number,timestamp,event::text from "
            <> quoteIdent eventTable
            <> " where index = "
            <> toQuery index
            <> " order by event_number"

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
        =<< query_
            conn
            ( "select id, event_number,timestamp,event::text from "
                <> quoteIdent eventTable
                <> " where event_number > "
                <> fromString (show lastEvent)
                <> " order by event_number"
            )

newtype EventQuery = EventQuery {getPgQuery :: PG.Query}
    deriving (Show, Generic)

mkEventsAfterQuery
    :: IsPgIndex index
    => EventTableName
    -> index
    -> EventNumber
    -> EventQuery
mkEventsAfterQuery eventTable index (EventNumber lastEvent) =
    EventQuery $
        "select id, event_number,timestamp,event::text from "
            <> quoteIdent eventTable
            <> " where index = "
            <> toQuery index
            <> " and event_number > "
            <> fromString (show lastEvent)
            <> " order by event_number"

mkEventQuery :: IsPgIndex index => EventTableName -> index -> EventQuery
mkEventQuery eventTable index =
    EventQuery $
        "select id, event_number,timestamp,event::text from "
            <> quoteIdent eventTable
            <> " where index = "
            <> toQuery index
            <> " order by event_number"

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
        "select count(*) > 0 from "
            <> quoteIdent eventTable
            <> " where event_number > "
            <> fromString (show lastEvent)

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
writeEvents conn eventTable index storedEvents = do
    _ <-
        executeMany
            conn
            ( "insert into "
                <> quoteIdent eventTable
                <> " (id, index, timestamp, event) \
                   \values (?, ?, ?, ?)"
            )
            ( fmap
                ( \x ->
                    ( storedUUID x
                    , toPgIndex index
                    , storedTimestamp x
                    , encode $ storedEvent x
                    )
                )
                storedEvents
            )
    foldl' max 0 . fmap fromOnly
        <$> query_
            conn
            ( "select coalesce(max(event_number),1) from "
                <> quoteIdent eventTable
            )

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

            giveBackConn :: IO ()
            giveBackConn = do
                PG.rollback conn
                putResource localPool conn
                t1 <- getCurrentTime
                pgt ^. field' @"logger" $
                    DbTransactionDuration (diffUTCTime t1 t0) (OneLineCallStack callStack)
        giveBackConn `catchAll` \_ -> do
            t1 <- getCurrentTime
            pgt ^. field' @"logger" $
                DbTransactionDuration (diffUTCTime t1 t0) (OneLineCallStack callStack)
            destroyResource (connectionPool pg) localPool conn

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
        pg ^. field @"logger" $
            WaitForConnectionDuration (diffUTCTime t1 t0) (OneLineCallStack callStack)
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

            giveBackConn :: IO ()
            giveBackConn = do
                readIORef transactionCompleted >>= \case
                    True -> PG.commit conn
                    False -> PG.rollback conn
                Pool.putResource localPool conn
                t1 <- getCurrentTime
                pgt ^. field' @"logger" $
                    DbTransactionDuration (diffUTCTime t1 t0) (OneLineCallStack callStack)
        giveBackConn `catchAll` \_ -> do
            t1 <- getCurrentTime
            pgt ^. field' @"logger" $
                DbTransactionDuration (diffUTCTime t1 t0) (OneLineCallStack callStack)
            destroyResource (connectionPool pg) localPool conn

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
        (Cursor.declareCursor conn (getPgQuery q))
        Cursor.closeCursor
        ( Stream.unfoldMany Unfold.fromList
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
                    . Stream.unfoldMany Unfold.fromList
                    . Stream.parMapM
                        ( Stream.maxThreads workers
                            . Stream.eager True
                            . Stream.ordered True
                        )
                        (traverse parseRowResult)
                    . Stream.foldMany (Fold.take taskSize Fold.toList)
                    $ Stream.fromList rows
    either throwM pure (sequence parsed)

getModel'
    :: forall e index m
     . (IsPgIndex index, FromJSON e, NFData e)
    => PostgresEventTrans index m e
    -> index
    -> IO m
getModel' pgt index = do
    NumberedModel model lastEventNo <- getCurrentState pgt index
    hasNewEvents <-
        queryHasEventsAfter
            (pgt ^. field @"transaction" . field @"connectionResource" . field @"resource")
            (pgt ^. field @"eventTableName")
            lastEventNo
    if hasNewEvents then fst <$> refreshModel pgt index else pure model

getCurrentState
    :: forall pg index model
     . ( IsPgIndex index
       , HasField' "modelIORef" pg (IORef (HashMap index (NumberedModel model)))
       , HasField' "seed" pg model
       )
    => pg
    -> index
    -> IO (NumberedModel model)
getCurrentState pg index =
    fromMaybe (NumberedModel (pg ^. field' @"seed") 0) . HM.lookup index
        <$> readIORef (pg ^. field' @"modelIORef")

refreshModel
    :: forall i m e
     . (IsPgIndex i, FromJSON e, NFData e)
    => PostgresEventTrans i m e
    -> i
    -> IO (m, EventNumber)
refreshModel pgt index = withExclusiveLock pgt index $ do
    -- refresh doesn't write any events but changes the state and thus needs a lock
    NumberedModel model lastEventNo <- getCurrentState pgt index
    let eventStream =
            mkEventStreamWithParseConcurrency
                (pgt ^. field @"parseConcurrency")
                (pgt ^. field @"chunkSize")
                (pgt ^. field @"transaction" . field @"connectionResource" . field @"resource")
                (mkEventsAfterQuery (pgt ^. field @"eventTableName") index lastEventNo)

        applyModel :: NumberedModel m -> (Stored e, EventNumber) -> NumberedModel m
        applyModel (NumberedModel m _) (ev, evNumber) =
            NumberedModel ((pgt ^. field @"app") m ev) evNumber

    newNumberedModel@(NumberedModel newModel lastNewEventNo) <-
        Stream.fold
            ( Fold.foldl'
                applyModel
                (NumberedModel model lastEventNo)
            )
            eventStream

    atomicModifyIORef
        (pgt ^. field @"modelIORef")
        (\a -> (HM.insert index newNumberedModel a, ()))
    pure (newModel, lastNewEventNo)

exclusiveLock :: IsPgIndex i => OngoingTransaction -> EventTableName -> i -> IO ()
exclusiveLock (OngoingTransaction connR _ _) _etName index = do
    -- We use advisory locks in favor of row level locks as we would not have the ability
    -- to lock an index before the first event is written with row level locks.
    void $
        ( query
            (Pool.resource connR)
            "SELECT pg_advisory_xact_lock(?)"
            (Only (fromIntegral (hash index) :: Int64))
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

    transactionalUpdate pg index cmd = withRunInIO $ \runInIO ->
        withIOTrans pg $ \pgt -> withExclusiveLock pgt index $ do
            m <- getModel' pgt index
            (returnFun, evs) <- runInIO $ cmd m
            storedEvs <- traverse toStored evs
            newNumberedModel <-
                uncurry NumberedModel
                    <$> concurrently
                        ( Stream.fold
                            (Fold.foldl' (pg ^. field @"app") m)
                            (Stream.fromList storedEvs)
                        )
                        ( writeEvents
                            (pgt ^. field @"transaction" . field @"connectionResource" . field @"resource")
                            (pg ^. field @"eventTableName")
                            index
                            storedEvs
                        )
            atomicModifyIORef
                (pg ^. field @"modelIORef")
                (\a -> (HM.insert index newNumberedModel a, ()))
            pure (model newNumberedModel, storedEvs, returnFun)
