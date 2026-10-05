-- | Postgres events with state as an IORef
module DomainDriven.Persistance.Postgres.Internal where

import Control.Concurrent (getNumCapabilities)
import Control.DeepSeq (NFData, force)
import Control.Exception (SomeAsyncException, evaluate)
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
import Data.Hashable (Hashable)
import Data.IORef
import Data.Int
import Data.List (sort, stripPrefix)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Pool.Introspection as Pool
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
import Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.Cursor qualified as Cursor
import Database.PostgreSQL.Simple.Types qualified as PGT
import DomainDriven.Persistance.Class
import DomainDriven.Persistance.Postgres.Types
import GHC.Generics (Generic)
import GHC.Stack
import Lens.Micro ((^.))
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Stream.Prelude (Stream)
import Streamly.Data.Stream.Prelude qualified as Stream
import Streamly.Data.Unfold qualified as Unfold
import Text.Read (readMaybe)
import UnliftIO (MonadUnliftIO (..), concurrently)
import Prelude

-- | Log entries for the persistance layer.
-- Not that OneLineCallStack has contains the CallStack, but prints only the call site.
data LogEntry
    = DbTransactionDuration NominalDiffTime OneLineCallStack
    | EventTableLockDuration NominalDiffTime OneLineCallStack
    | EventTableMigrationDuration NominalDiffTime EventTableName
    | WaitForConnectionDuration NominalDiffTime OneLineCallStack
    | -- | Startup is waiting for another instance to finish verifying or migrating the
      -- event tables of this base name.
      WaitingForMigrationLock EventTableBaseName
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
    getModel pg index = liftIO $ do
        NumberedModel cached lastEventNo <- getCurrentState pg index
        -- Release this connection before refreshing; the pool may contain one.
        hasNewEvents <- withPooledConnection pg $ \conn ->
            queryHasEventsAfter conn (pg ^. field @"eventTableName") index lastEventNo
        if hasNewEvents
            then model <$> withIOTrans pg (`refreshModel` index)
            else pure cached

    getEventList pg index = withPooledConnection pg $ \conn ->
        fmap fst
            <$> queryEventsWithParseConcurrency
                (pg ^. field @"parseConcurrency")
                (pg ^. field @"chunkSize")
                conn
                (pg ^. field @"eventTableName")
                index

    getEventStream pg = withStreamReadTransaction pg . flip getEventStream'

-- | The base name and 'TableName' version at the bottom of the chain.
eventTableBase :: EventTable -> (EventTableBaseName, EventTableVersion)
eventTableBase = \case
    MigrateTo _ _ prev -> eventTableBase prev
    TableName base v -> (base, v)

-- | The 'MigrateTo' steps of the chain, oldest first.
eventTableSteps :: EventTable -> [(EventTableVersion, EventMigration)]
eventTableSteps = reverse . newestFirst
  where
    newestFirst :: EventTable -> [(EventTableVersion, EventMigration)]
    newestFirst = \case
        MigrateTo v mig prev -> (v, mig) : newestFirst prev
        TableName{} -> []

-- | The version of the current (newest) table of the chain.
eventTableVersion :: EventTable -> EventTableVersion
eventTableVersion = \case
    MigrateTo v _ _ -> v
    TableName _ v -> v

-- | Non-empty and only @[a-zA-Z0-9_]@, so the name is safe both quoted in SQL and
-- spliced into a regular expression.
isSafeIdentifier :: String -> Bool
isSafeIdentifier name = not (null name) && all isSafeChar name
  where
    isSafeChar :: Char -> Bool
    isSafeChar c = isAsciiLower c || isAsciiUpper c || isDigit c || c == '_'

-- | Render the current table name. Use 'validateEventTable' to check the chain
-- before using the name; persistence entry points validate before connecting.
getEventTableName :: EventTable -> EventTableName
getEventTableName et =
    eventTableNameFor (fst $ eventTableBase et) (eventTableVersion et)

-- | Check a raw table name before database work. Throws 'MigrationError'.
validateEventTableName :: MonadThrow m => EventTableName -> m ()
validateEventTableName name
    | not (isSafeIdentifier name) = throwM (InvalidEventTableName name)
    | length name > maxEventTableNameLength = throwM (EventTableNameTooLong name)
    | otherwise = pure ()

-- | Check an 'EventTable' chain without touching the database: the base name must be
-- non-empty and contain only @[a-zA-Z0-9_]@, the 'TableName' version must be at least 1,
-- the current table name must be at most 'maxEventTableNameLength' characters, and the
-- 'MigrateTo' steps must be numbered consecutively from one above the 'TableName'
-- version. Throws 'MigrationError'. 'postgresWriteModel' runs this before connecting.
validateEventTable :: MonadThrow m => EventTable -> m ()
validateEventTable et = do
    unless (isSafeIdentifier base) $ throwM $ InvalidEventTableBaseName base
    unless (baseVersion >= 1) $ throwM $ InvalidEventTableVersion base baseVersion
    unless (length currentName <= maxEventTableNameLength) $
        throwM $
            EventTableNameTooLong currentName
    for_ (zip [baseVersion + 1 ..] (fst <$> eventTableSteps et)) $ \(expected, actual) ->
        unless (expected == actual) $ throwM $ MisnumberedMigration base expected actual
  where
    base :: EventTableBaseName
    baseVersion :: EventTableVersion
    (base, baseVersion) = eventTableBase et

    currentName :: EventTableName
    currentName = eventTableNameFor base (eventTableVersion et)

-- | Create the table required for storing state and events, if they do not yet exist.
createEventTable :: PostgresEventTrans index model event -> IO ()
createEventTable pgt = do
    void $
        createEventTable'
            (pgt ^. #transaction . #connectionResource . #resource)
            (pgt ^. #eventTableName)

createEventTable' :: Connection -> EventTableName -> IO ()
createEventTable' conn = createEventTableInSchema conn Nothing

createEventTableInSchema :: Connection -> Maybe String -> EventTableName -> IO ()
createEventTableInSchema conn schema eventTable = do
    validateEventTableName eventTable
    void . execute_ conn $
        "create table if not exists "
            <> tableName
            <> " \
               \( id uuid primary key\
               \, index varchar not null\
               \, event_number bigint not null generated always as identity\
               \, timestamp timestamptz not null default now()\
               \, event jsonb not null\
               \);"
    -- Postgres may truncate generated names, so match the index definition.
    hasIndex <-
        query
            conn
            "select exists (select 1 from pg_indexes \
            \where schemaname = coalesce(?, current_schema()) and tablename = ? \
            \and indexdef like '%(index, event_number)')"
            (schema, eventTable)
            >>= \case
                [Only found] -> pure found
                unexpected -> fail $ "Unexpected index query result: " <> show unexpected
    unless hasIndex . void . execute_ conn $
        "create index on " <> tableName <> " (index, event_number)"
  where
    tableName :: Query
    tableName = maybe "" ((<> ".") . quoteIdent) schema <> quoteIdent eventTable

retireTable :: Connection -> EventTableName -> IO ()
retireTable conn tableName = do
    schema <- eventTableSchema conn tableName
    let retiredFunction :: Query
        retiredFunction = quoteIdent schema <> ".retired_table"
    void . execute_ conn $
        "create or replace function " <> retiredFunction <> "() returns trigger as \
        \$$ begin raise exception 'Event table has been retired.'; end; $$ \
        \language plpgsql;"
    void $
        execute_ conn $
            "create trigger retired before insert on "
                <> quoteIdent schema
                <> "."
                <> quoteIdent tableName
                <> " execute procedure "
                <> retiredFunction
                <> "()"

eventTableSchema :: Connection -> EventTableName -> IO String
eventTableSchema conn tableName =
    query
        conn
        "select n.nspname::text from pg_catalog.pg_class c \
        \join pg_catalog.pg_namespace n on n.oid = c.relnamespace \
        \where c.oid = to_regclass(quote_ident(?))"
        (Only tableName)
        >>= \case
            [Only schema] -> pure schema
            unexpected -> fail $ "Unexpected event table schema query result: " <> show unexpected

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
--
-- Writers sharing the database must all run the same domaindriven-core version.
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

-- | Setup the persistance model, validating the 'EventTable' chain and running any
-- outstanding migrations (see 'runMigrations'). Throws 'MigrationError' if the chain is
-- invalid or cannot be applied to the database.
--
-- Writers sharing the database must all run the same domaindriven-core version.
postgresWriteModel
    :: HasCallStack
    => Pool Connection
    -> EventTable
    -> (model -> Stored event -> model)
    -> model
    -> IO (PostgresEvent index model event)
postgresWriteModel = postgresWriteModelWith id

-- | Like 'postgresWriteModel', applying a modifier to the persistance model before
-- migrations run, so that for instance a custom logger receives the startup entries.
postgresWriteModelWith
    :: HasCallStack
    => (PostgresEvent index model event -> PostgresEvent index model event)
    -> Pool Connection
    -> EventTable
    -> (model -> Stored event -> model)
    -> model
    -> IO (PostgresEvent index model event)
postgresWriteModelWith modify pool eventTable app' seed' = do
    validateEventTable eventTable
    pg <- modify <$> createPostgresPersistance pool (getEventTableName eventTable) app' seed'
    withIOTrans pg $ \pgt ->
        runMigrations (pgt ^. field @"logger") (pgt ^. field @"transaction") eventTable
    pure pg

-- | Versions in the first schema on the search path containing tables for this base,
-- ascending. Prefix bases (@foo@ vs @foo_v2@) do not match each other.
-- The base must pass 'isSafeIdentifier', since it is spliced into a regular expression.
existingEventTableVersions :: Connection -> EventTableBaseName -> IO [EventTableVersion]
existingEventTableVersions conn base = do
    names <-
        query
            conn
            "with event_tables as ( \
            \ select c.relname, array_position(current_schemas(true), n.nspname) as schema_position \
            \ from pg_catalog.pg_class c \
            \ join pg_catalog.pg_namespace n on n.oid = c.relnamespace \
            \ where c.relkind in ('r', 'p') \
            \   and pg_catalog.pg_table_is_visible(c.oid) \
            \   and c.relname ~ ? \
            \) select relname::text from event_tables \
            \where schema_position = (select min(schema_position) from event_tables)"
            (Only $ "^" <> eventTablePrefix base <> "[1-9][0-9]*$")
    pure . sort $
        mapMaybe (readMaybe <=< stripPrefix (eventTablePrefix base) . fromOnly) names

-- | Serialize migrators of one base name until the transaction ends.
migrationLock :: (LogEntry -> IO ()) -> Connection -> EventTableBaseName -> IO ()
migrationLock logger conn base = do
    acquired <- tryAdvisoryXactLock conn key
    unless acquired $ do
        logSafely logger $ WaitingForMigrationLock base
        advisoryXactLock conn key
  where
    -- Table keys hash a bare table name, which cannot contain a slash.
    key :: String
    key = "domaindriven/migration/" <> base

-- | Whether the connection is in a transaction that has made changes. The migration
-- transaction has created a table by the time a migration function runs, so 'False'
-- afterwards means the function committed or rolled back. Requires PostgreSQL 13.
inWriteTransaction :: Connection -> IO Bool
inWriteTransaction conn =
    query_ conn "select pg_current_xact_id_if_assigned() is not null" >>= \case
        [Only inTransaction] -> pure inTransaction
        unexpected -> fail $ "Unexpected transaction query result: " <> show unexpected

-- | Bring the database forward to the code's current version. Runs in the transaction of
-- the given 'OngoingTransaction', which must be freshly begun (nothing may have been
-- executed in it yet). Throws 'MigrationError' for an invalid chain ('validateEventTable').
--
-- The highest existing version is the database's version, provided the previous
-- existing table is retired. Otherwise startup fails with 'IncompleteMigration'.
-- A database without tables for the base name gets only the code's current table; no
-- migration function runs. Otherwise each step above the database's version locks the
-- previous table against writers, creates the new table, runs the migration function and
-- retires the previous table. A database above the code's version or below its
-- 'TableName' version throws 'MigrationError'.
runMigrations :: (LogEntry -> IO ()) -> OngoingTransaction -> EventTable -> IO ()
runMigrations logger trans et = do
    validateEventTable et
    -- Every statement below must see what was committed while we waited for the locks,
    -- so pin the isolation level rather than relying on the server default.
    void $ execute_ conn "set transaction isolation level read committed"
    migrationLock logger conn base
    (reverse <$> existingEventTableVersions conn base) >>= \case
        [] -> createEventTable' conn (eventTableNameFor base codeVersion)
        dbVersion : previousVersions -> do
            when (dbVersion > codeVersion) . throwM $ DatabaseAheadOfCode base dbVersion codeVersion
            when (dbVersion < baseVersion) . throwM $
                DatabaseBelowBaseVersion base dbVersion baseVersion
            for_ (take 1 previousVersions) $ \prevVersion -> do
                let prevName :: EventTableName
                    prevName = eventTableNameFor base prevVersion
                query
                    conn
                    "select exists (select 1 from pg_catalog.pg_trigger \
                    \where tgrelid = to_regclass(quote_ident(?)) and tgname = 'retired' \
                    \and tgenabled in ('O', 'A'))"
                    (Only prevName)
                    >>= \case
                        [Only True] -> pure ()
                        [Only False] -> throwM $ IncompleteMigration prevName (eventTableNameFor base dbVersion)
                        unexpected -> fail $ "Unexpected retirement query result: " <> show unexpected
            for_ (dropWhile ((<= dbVersion) . fst) (eventTableSteps et)) migrateStep
  where
    conn :: Connection
    conn = trans ^. field @"connectionResource" . field @"resource"

    base :: EventTableBaseName
    baseVersion :: EventTableVersion
    (base, baseVersion) = eventTableBase et

    codeVersion :: EventTableVersion
    codeVersion = eventTableVersion et

    migrateStep :: (EventTableVersion, EventMigration) -> IO ()
    migrateStep (version, mig) = do
        let prevName :: EventTableName
            prevName = eventTableNameFor base (version - 1)

            newName :: EventTableName
            newName = eventTableNameFor base version
        -- Drain the previous table's writers, which hold this key shared, and block new
        -- ones until the old table is retired.
        exclusiveTableLock conn prevName
        t0 <- getCurrentTime
        schema <- eventTableSchema conn prevName
        createEventTableInSchema conn (Just schema) newName
        mig prevName newName conn
        stillInTransaction <- inWriteTransaction conn
        unless stillInTransaction . throwM $ MigrationEndedTransaction newName
        retireTable conn prevName
        t1 <- getCurrentTime
        logSafely logger $ EventTableMigrationDuration (diffUTCTime t1 t0) newName

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
    validateEventTableName eventTable
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
                WaitingForMigrationLock base ->
                    putStrLn $ "[DomainDriven] waiting for migration lock on " <> base
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
    indexText <- indexParam index
    parseEventRows workers chunkSize
        =<< query conn (eventsQuery eventTable) (indexText, 0 :: Int64)

eventsQuery :: EventTableName -> PG.Query
eventsQuery eventTable =
    "select id, event_number, timestamp, event::text from "
        <> quoteIdent eventTable
        <> " where index = ? and event_number > ? order by event_number"

-- | Reject NULs, which libpq would truncate.
indexParam :: IsPgIndex index => index -> IO Text
indexParam index
    | T.any (== '\0') indexText = throwM (ValueError "Index values must not contain NUL bytes")
    | otherwise = pure indexText
  where
    indexText :: Text
    indexText = toPgIndex index

queryHasEventsAfter
    :: IsPgIndex index
    => Connection
    -> EventTableName
    -> index
    -> EventNumber
    -> IO Bool
queryHasEventsAfter conn eventTable index (EventNumber lastEvent) = do
    indexText <- indexParam index
    result <-
        query
            conn
            ( "select exists (select 1 from "
                <> quoteIdent eventTable
                <> " where index = ? and event_number > ?)"
            )
            (indexText, lastEvent)
    case result of
        [Only hasEvents] -> pure hasEvents
        unexpected -> fail $ "Unexpected freshness query result: " <> show unexpected

-- | Insert events and return the highest event number, or 0 for none.
--
-- Hold the shared table key and the exclusive index key ('writerLocks') from
-- model read through commit and keep the event sequence at @CACHE 1@;
-- otherwise caches can miss events and migrations can copy incompletely.
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
    indexText <- indexParam index
    eventNumbers <-
        returning
            conn
            ( "insert into "
                <> quoteIdent eventTable
                <> " (id, index, timestamp, event) \
                   \values (?, ?, ?, ?) returning event_number"
            )
            ( fmap
                ( \x ->
                    ( storedUUID x
                    , indexText
                    , storedTimestamp x
                    , encode $ storedEvent x
                    )
                )
                storedEvents
            )
    pure $ foldl' max 0 (fmap fromOnly eventNumbers)

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
            (pgt ^. #eventTableName)
            index
            0

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
        validateEventTableName (pg ^. field @"eventTableName")
        (connR, localPool) <- takeResource (connectionPool pg)
        t0 <- getCurrentTime
        let conn = Pool.resource connR
        beginResult <- tryIO (PG.begin conn)
        case beginResult of
            Left beginError -> do
                destroyConnection (connectionPool pg) localPool conn
                throwM beginError
            Right () -> pure ()
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
        let OngoingTransaction connR localPool t0 = pgt ^. field' @"transaction"
            conn = Pool.resource connR
        rollbackResult <- tryIO (PG.rollback conn)
        case rollbackResult of
            Right () -> releaseConnection (connectionPool pg) localPool conn
            Left _ -> destroyConnection (connectionPool pg) localPool conn
        t1 <- getCurrentTime
        logSafely (pgt ^. field' @"logger") $
            DbTransactionDuration (diffUTCTime t1 t0) (OneLineCallStack callStack)

withPooledConnection
    :: HasCallStack
    => PostgresEvent index model event
    -> (Connection -> IO a)
    -> IO a
withPooledConnection pg f = do
    validateEventTableName (pg ^. field @"eventTableName")
    t0 <- getCurrentTime
    withResource (connectionPool pg) $ \connR -> do
        t1 <- getCurrentTime
        logSafely (pg ^. field @"logger") $
            WaitForConnectionDuration (diffUTCTime t1 t0) (OneLineCallStack callStack)
        f (Pool.resource connR)

withIOTrans
    :: forall a index model event
     . HasCallStack
    => PostgresEvent index model event
    -> (PostgresEventTrans index model event -> IO a)
    -> IO a
withIOTrans pg f = mask $ \restore -> do
    validateEventTableName (pg ^. field @"eventTableName")
    (connR, localPool) <- do
        t0 <- getCurrentTime
        r@(acquiredConnR, acquiredLocalPool) <- takeResource (connectionPool pg)
        t1 <- getCurrentTime
        waitLogResult <- tryIO $
            logSafely (pg ^. field @"logger") $
                WaitForConnectionDuration (diffUTCTime t1 t0) (OneLineCallStack callStack)
        case waitLogResult of
            Right () -> pure r
            Left waitLogError -> do
                releaseConnection
                    (connectionPool pg)
                    acquiredLocalPool
                    (Pool.resource acquiredConnR)
                throwM waitLogError
    prepareResult <- tryIO (prepareTransaction connR localPool)
    pgt <- case prepareResult of
        Right transaction -> pure transaction
        Left prepareError -> do
            destroyConnection (connectionPool pg) localPool (Pool.resource connR)
            throwM prepareError
    bodyResult <- tryIO (restore (f pgt))
    case bodyResult of
        Left bodyError -> do
            rollbackAndRelease pgt
            throwM bodyError
        Right result -> do
            let OngoingTransaction committedConnR committedLocalPool _ = pgt ^. field' @"transaction"
                conn = Pool.resource committedConnR
            commitResult <- tryIO (PG.commit conn)
            case commitResult of
                Left commitError -> do
                    destroyConnection (connectionPool pg) committedLocalPool conn
                    logTransactionDuration pgt
                    throwM commitError
                Right () -> do
                    releaseConnection (connectionPool pg) committedLocalPool conn
                    logTransactionDuration pgt
                    pure result
  where
    rollbackAndRelease :: PostgresEventTrans index model event -> IO ()
    rollbackAndRelease pgt = do
        let OngoingTransaction connR localPool _ = pgt ^. field' @"transaction"
            conn = Pool.resource connR
        rollbackResult <- tryIO (PG.rollback conn)
        case rollbackResult of
            Right () -> releaseConnection (connectionPool pg) localPool conn
            Left _ -> destroyConnection (connectionPool pg) localPool conn
        logTransactionDuration pgt

    logTransactionDuration :: PostgresEventTrans index model event -> IO ()
    logTransactionDuration pgt = do
        let OngoingTransaction _ _ t0 = pgt ^. field' @"transaction"
        t1 <- getCurrentTime
        logSafely (pgt ^. field' @"logger") $
            DbTransactionDuration (diffUTCTime t1 t0) (OneLineCallStack callStack)

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

mkEventStreamWithParseConcurrency
    :: (FromJSON event, NFData event, IsPgIndex index)
    => ParseConcurrency
    -> ChunkSize
    -> Connection
    -> EventTableName
    -> index
    -> EventNumber
    -- ^ Start after this event number
    -> Stream IO (Stored event, EventNumber)
mkEventStreamWithParseConcurrency parseConcurrency chunkSize conn eventTable index (EventNumber after) = do
    let step :: Cursor.Cursor -> IO (Maybe (Seq EventRowOut, Cursor.Cursor))
        step cursor = do
            r <- Cursor.foldForward cursor chunkSize (\a r -> pure (a :|> r)) Seq.Empty
            case r of
                Left Seq.Empty -> pure Nothing
                Left a -> pure $ Just (a, cursor)
                Right a -> pure $ Just (a, cursor)

        -- Cursors cannot bind parameters; render them with libpq.
        declare :: IO Cursor.Cursor
        declare = do
            indexText <- indexParam index
            rendered <- formatQuery conn (eventsQuery eventTable) (indexText, after)
            Cursor.declareCursor conn (PGT.Query rendered)

    Stream.bracketIO
        declare
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

getNumberedModel'
    :: forall e index m
     . (IsPgIndex index, FromJSON e, NFData e)
    => PostgresEventTrans index m e
    -> index
    -> IO (NumberedModel m)
getNumberedModel' pgt index = do
    current@(NumberedModel _ lastEventNo) <- getCurrentState pgt index
    hasNewEvents <-
        queryHasEventsAfter
            (pgt ^. field @"transaction" . field @"connectionResource" . field @"resource")
            (pgt ^. field @"eventTableName")
            index
            lastEventNo
    if hasNewEvents
        then refreshModel pgt index
        else pure current

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
    -> IO (NumberedModel m)
refreshModel pgt index = withExclusiveLock pgt index $ do
    -- refresh doesn't write any events but changes the state and thus needs a lock
    NumberedModel model lastEventNo <- getCurrentState pgt index
    let eventStream =
            mkEventStreamWithParseConcurrency
                (pgt ^. field @"parseConcurrency")
                (pgt ^. field @"chunkSize")
                (pgt ^. field @"transaction" . field @"connectionResource" . field @"resource")
                (pgt ^. field @"eventTableName")
                index
                lastEventNo

        applyModel :: NumberedModel m -> (Stored e, EventNumber) -> NumberedModel m
        applyModel (NumberedModel m _) (ev, evNumber) =
            NumberedModel ((pgt ^. field @"app") m ev) evNumber

    newNumberedModel <-
        Stream.fold
            ( Fold.foldl'
                applyModel
                (NumberedModel model lastEventNo)
            )
            eventStream

    publishNumberedModel (pgt ^. field @"modelIORef") index newNumberedModel
    pure newNumberedModel

publishNumberedModel
    :: Hashable index
    => IORef (HashMap index (NumberedModel model))
    -> index
    -> NumberedModel model
    -> IO ()
publishNumberedModel ref index candidate@(NumberedModel _ candidateEventNumber) =
    atomicModifyIORef' ref $ \models ->
        case HM.lookup index models of
            Just (NumberedModel _ currentEventNumber)
                | currentEventNumber >= candidateEventNumber -> (models, ())
            Nothing -> (HM.insert index candidate models, ())
            Just _ -> (HM.insert index candidate models, ())

exclusiveLock :: IsPgIndex i => OngoingTransaction -> EventTableName -> i -> IO ()
exclusiveLock (OngoingTransaction connR _ _) etName index = do
    -- Advisory locks cover empty indices; Postgres derives stable keys.
    indexText <- indexParam index
    void
        ( query
            (Pool.resource connR)
            "select pg_advisory_xact_lock(hashtextextended(?, hashtextextended(?, 0)))"
            (indexText, etName)
            :: IO [Only ()]
        )

-- | Command locks: the table key shared plus the index key exclusive.
-- Default to a 60-second lock timeout, scoped to this transaction, because
-- a nested command can wait behind a migration waiting for its outer command.
-- Preserve any finite timeout configured on the connection.
writerLocks :: IsPgIndex i => OngoingTransaction -> EventTableName -> i -> IO ()
writerLocks (OngoingTransaction connR _ _) etName index = do
    indexText <- indexParam index
    void
        ( query_
            (Pool.resource connR)
            "select set_config('lock_timeout', \
            \case current_setting('lock_timeout') \
            \when '0' then '60s' else current_setting('lock_timeout') end, true)"
            :: IO [Only Text]
        )
    void
        ( query
            (Pool.resource connR)
            "select pg_advisory_xact_lock_shared(hashtextextended(?, 0)), \
            \pg_advisory_xact_lock(hashtextextended(?, hashtextextended(?, 0)))"
            (etName, indexText, etName)
            :: IO [((), ())]
        )

-- | The whole-table lock: waits for every in-flight command on the table (they
-- hold this key shared) and blocks new ones until the transaction ends.
exclusiveTableLock :: Connection -> EventTableName -> IO ()
exclusiveTableLock = advisoryXactLock

-- | Take the exclusive transaction-level advisory lock for a text key, waiting for it.
advisoryXactLock :: Connection -> String -> IO ()
advisoryXactLock conn key =
    void
        ( query
            conn
            "select pg_advisory_xact_lock(hashtextextended(?, 0))"
            (Only key)
            :: IO [Only ()]
        )

-- | Like 'advisoryXactLock', but gives up at once if the key is held by someone else.
tryAdvisoryXactLock :: Connection -> String -> IO Bool
tryAdvisoryXactLock conn key =
    query conn "select pg_try_advisory_xact_lock(hashtextextended(?, 0))" (Only key) >>= \case
        [Only acquired] -> pure acquired
        unexpected -> fail $ "Unexpected lock query result: " <> show unexpected

withLockLogging
    :: HasCallStack => PostgresEventTrans i m e -> IO () -> IO a -> IO a
withLockLogging pgt takeLocks a = do
    takeLocks
    t0 <- getCurrentTime
    r <- a
    t1 <- getCurrentTime
    logSafely (pgt ^. field' @"logger") $
        EventTableLockDuration (diffUTCTime t1 t0) (OneLineCallStack callStack)
    pure r

withExclusiveLock
    :: (HasCallStack, IsPgIndex i) => PostgresEventTrans i m e -> i -> IO a -> IO a
withExclusiveLock pgt index =
    withLockLogging
        pgt
        (exclusiveLock (pgt ^. field' @"transaction") (pgt ^. field @"eventTableName") index)

withWriterLocks
    :: (HasCallStack, IsPgIndex i) => PostgresEventTrans i m e -> i -> IO a -> IO a
withWriterLocks pgt index =
    withLockLogging
        pgt
        (writerLocks (pgt ^. field' @"transaction") (pgt ^. field @"eventTableName") index)

instance (IsPgIndex i, ToJSON e, FromJSON e, NFData e) => WriteModel (PostgresEvent i m e) where
    postUpdateHook pg i m e = liftIO $ (pg ^. field @"updateHook") pg i m e

    transactionalUpdate pg index cmd = withRunInIO $ \runInIO -> do
        (newNumberedModel, storedEvs, returnFun) <-
            withIOTrans pg $ \pgt ->
                withWriterLocks pgt index $ do
                    NumberedModel m previousEventNumber <- getNumberedModel' pgt index
                    (returnFun, evs) <- runInIO $ cmd m
                    storedEvs <- traverse toStored evs
                    (newModel, batchEventNumber) <-
                        concurrently
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
                    let newNumberedModel =
                            NumberedModel
                                newModel
                                (max previousEventNumber batchEventNumber)
                    pure (newNumberedModel, storedEvs, returnFun)
        publishNumberedModel (pg ^. field @"modelIORef") index newNumberedModel
        pure (model newNumberedModel, storedEvs, returnFun)

tryIO :: IO a -> IO (Either SomeException a)
tryIO = try

releaseConnection :: Pool Connection -> LocalPool Connection -> Connection -> IO ()
releaseConnection pool localPool conn =
    Pool.putResource localPool conn `catchAll` \exception -> do
        destroyConnection pool localPool conn
        rethrowAsyncException exception

destroyConnection :: Pool Connection -> LocalPool Connection -> Connection -> IO ()
destroyConnection pool localPool conn =
    destroyResource pool localPool conn `catchAll` rethrowAsyncException

logSafely :: (LogEntry -> IO ()) -> LogEntry -> IO ()
logSafely logEntry entry = logEntry entry `catchAll` rethrowAsyncException

rethrowAsyncException :: SomeException -> IO ()
rethrowAsyncException exception = case fromException exception :: Maybe SomeAsyncException of
    Just _ -> throwM exception
    Nothing -> pure ()
