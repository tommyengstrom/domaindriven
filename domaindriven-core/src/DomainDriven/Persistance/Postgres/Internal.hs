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
import Data.List (intercalate, sort, sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Pool.Introspection as Pool
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.String
import Data.Time
import Data.Traversable (for)
import Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.Cursor qualified as Cursor
import DomainDriven.Persistance.Class
import DomainDriven.Persistance.Postgres.Types
import GHC.Generics (Generic)
import GHC.Stack
import Lens.Micro
    ( to
    , (&)
    , (<>~)
    , (^.)
    )
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
    | -- | Emitted right before the migration lock is requested at startup, so a startup
      -- that hangs behind another instance's migration says why.
      WaitingForMigrationLock EventTableBaseName
    | -- | The current table was created on a database without tables for the base name;
      -- the given number of migrations were recorded as applied without running.
      EventTableBootstrapped EventTableName Int
    | -- | An existing table without a metadata row was recorded as the current version.
      EventTableAdopted EventTableName
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

-- | The base name and 'TableName' version at the bottom of the chain.
eventTableBase :: EventTable -> (EventTableBaseName, EventTableVersion)
eventTableBase = \case
    MigrateWith _ _ prev -> eventTableBase prev
    TableName base v -> (base, v)

-- | The 'MigrateWith' steps of the chain, oldest first, each with the version it produces.
eventTableSteps :: EventTable -> [(EventTableVersion, MigrationTag, EventMigration)]
eventTableSteps et = zipWith (\v (t, mig) -> (v, t, mig)) [snd (eventTableBase et) + 1 ..] (go et [])
  where
    go (MigrateWith t mig prev) acc = go prev ((t, mig) : acc)
    go TableName{} acc = acc

-- | The version of the current (newest) table of the chain.
eventTableVersion :: EventTable -> EventTableVersion
eventTableVersion = \case
    MigrateWith _ _ prev -> eventTableVersion prev + 1
    TableName _ v -> v

-- | Table name for a version of a base name: @\<base\>_v\<version\>@.
eventTableNameFor :: EventTableBaseName -> EventTableVersion -> EventTableName
eventTableNameFor base v = base <> "_v" <> show v

isValidBaseName :: EventTableBaseName -> Bool
isValidBaseName name = not (null name) && all isValidChar name
  where
    isValidChar c = isAsciiLower c || isAsciiUpper c || isDigit c || c == '_'

-- | Name of the current table of the chain.
getEventTableName :: EventTable -> EventTableName
getEventTableName et = validate $ eventTableNameFor (fst $ eventTableBase et) (eventTableVersion et)
  where
    validate name
        | isValidBaseName name = name
        | otherwise =
            error $
                "[DomainDriven] Invalid event table name: "
                    <> show name
                    <> ". Names must be non-empty and contain only [a-zA-Z0-9_]."

-- | Check an 'EventTable' chain without touching the database: the base name must be
-- non-empty and contain only @[a-zA-Z0-9_]@, the 'TableName' version must be at least 1,
-- and the migration tags must be non-empty and unique within the chain. Throws
-- 'MigrationError'. 'postgresWriteModel' runs this before connecting.
validateEventTable :: MonadThrow m => EventTable -> m ()
validateEventTable et = do
    unless (isValidBaseName base) $ throwM $ InvalidEventTableBaseName base
    unless (baseVersion >= 1) $ throwM $ InvalidEventTableVersion base baseVersion
    for_ steps $ \(v, t, _) -> when (null t) $ throwM $ InvalidMigrationTag base v t
    for_ duplicateTags $ \(t, vs) -> throwM $ DuplicateMigrationTag base t vs
  where
    (base, baseVersion) = eventTableBase et
    steps = eventTableSteps et
    duplicateTags =
        filter ((> 1) . length . snd)
            . M.toList
            $ M.fromListWith (flip (<>)) [(t, [v]) | (v, t, _) <- steps]

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

-- | The columns every event table has; used to recognise event tables when adopting
-- tables that are not recorded in the metadata table.
eventTableColumns :: [String]
eventTableColumns = ["id", "index", "event_number", "timestamp", "event"]

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

-- | Whether the table has been retired ('retireTable'), i.e. rejects inserts.
isRetired :: Connection -> EventTableName -> IO Bool
isRetired conn tableName =
    queryBool
        conn
        "select exists \
        \(select 1 from information_schema.triggers \
        \ where trigger_schema = current_schema() \
        \   and event_object_table = ? \
        \   and trigger_name = 'retired')"
        (Only tableName)

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

-- | Setup the persistance model, verifying the 'EventTable' chain against the database
-- and running any outstanding migrations (see 'runMigrations'). Throws 'MigrationError'
-- if the chain is invalid or disagrees with the database.
postgresWriteModel
    :: HasCallStack
    => Pool Connection
    -> EventTable
    -> (model -> Stored event -> model)
    -> model
    -> IO (PostgresEvent index model event)
postgresWriteModel pool eventTable app' seed' = do
    validateEventTable eventTable
    pg <- createPostgresPersistance pool (getEventTableName eventTable) app' seed'
    withResource pool $ ensureMigrationsTable . Pool.resource
    withIOTrans pg $ \pgt ->
        runMigrations (pgt ^. field @"logger") (pgt ^. field @"transaction") eventTable
    pure pg

--------------------------------------------------------------------------------
-- Migration metadata
--------------------------------------------------------------------------------

-- | Name of the table recording, per event table base name, which versions exist and
-- which migration (by tag) produced them.
migrationsTableName :: EventTableName
migrationsTableName = "domaindriven_migrations"

-- | Advisory lock key serialising the creation of the metadata table.
migrationsTableLockKey :: Int64
migrationsTableLockKey = fromIntegral $ hash ("domaindriven/migrations-table" :: String)

-- | Advisory lock key serialising migrators of one base name. A tagged tuple, so that the
-- key cannot coincide with the writer locks (@hash (tableName, index)@).
migrationLockKey :: EventTableBaseName -> Int64
migrationLockKey base = fromIntegral $ hash ("domaindriven/migration" :: String, base)

-- | Create the metadata table if it does not exist, in its own short, committed
-- transaction (so it never holds up other services' startups behind a slow migration).
ensureMigrationsTable :: Connection -> IO ()
ensureMigrationsTable conn = do
    exists <- tableExists conn migrationsTableName
    unless exists . withTransaction conn $ do
        -- `create table if not exists` still races on the catalog; serialise creators.
        advisoryXactLock conn migrationsTableLockKey
        void . execute_ conn $
            "create table if not exists "
                <> quoteIdent migrationsTableName
                <> " \
                   \( base_name text not null\
                   \, version int not null\
                   \, tag text null\
                   \, origin text not null\
                   \, created_at timestamptz not null default now()\
                   \, primary key (base_name, version)\
                   \, unique (base_name, tag)\
                   \)"

-- | A row of the metadata table.
data MigrationRow = MigrationRow
    { version :: EventTableVersion
    , tag :: Maybe MigrationTag
    -- ^ Nothing: unknown (adopted table, or the base version of a bootstrapped chain)
    , origin :: MigrationOrigin
    }
    deriving (Show, Eq, Generic)

-- | What the database knows about a base name.
data EventTableState = EventTableState
    { recorded :: [MigrationRow]
    -- ^ Metadata rows, ascending by version
    , existingVersions :: [EventTableVersion]
    -- ^ Versions for which a table exists in the current schema, ascending
    }
    deriving (Show, Eq, Generic)

-- | The current version according to the metadata (the highest recorded one).
eventTableStateCurrent :: EventTableState -> Maybe EventTableVersion
eventTableStateCurrent st = case st ^. #recorded of
    [] -> Nothing
    rows -> Just $ maximum [v | MigrationRow v _ _ <- rows]

readEventTableState :: Connection -> EventTableBaseName -> IO EventTableState
readEventTableState conn base = do
    rows <-
        query
            conn
            ( "select version, tag, origin from "
                <> quoteIdent migrationsTableName
                <> " where base_name = ? order by version"
            )
            (Only base)
    recorded <- for rows $ \(v, t, o) -> case parseMigrationOrigin o of
        Just origin -> pure $ MigrationRow v t origin
        Nothing ->
            throwM . InvalidMigrationMetadata base $
                "unknown origin " <> show o <> " recorded for version " <> show v
    existingVersions <- existingEventTableVersions conn base
    pure EventTableState{recorded, existingVersions}

-- | Versions of the base name for which a table @\<base\>_v\<n\>@ exists in the current
-- schema, ascending. Prefix bases (@foo@ vs @foo_v2@) do not match each other.
existingEventTableVersions :: Connection -> EventTableBaseName -> IO [EventTableVersion]
existingEventTableVersions conn base = do
    names <-
        query
            conn
            "select table_name::text from information_schema.tables \
            \ where table_schema = current_schema() \
            \   and table_type = 'BASE TABLE' \
            \   and table_name::text ~ ?"
            (Only $ "^" <> base <> "_v[1-9][0-9]*$")
    pure . sort $ mapMaybe (readMaybe . drop (length base + 2) . fromOnly) names

tableExists :: Connection -> EventTableName -> IO Bool
tableExists conn tableName =
    queryBool
        conn
        "select exists \
        \(select 1 from information_schema.tables \
        \ where table_schema = current_schema() and table_name = ?)"
        (Only tableName)

tableColumns :: Connection -> EventTableName -> IO [String]
tableColumns conn tableName =
    fmap fromOnly
        <$> query
            conn
            "select column_name::text from information_schema.columns \
            \ where table_schema = current_schema() and table_name = ?"
            (Only tableName)

currentSchema :: Connection -> IO String
currentSchema conn =
    query_ conn "select current_schema()::text" >>= \case
        [Only s] -> pure s
        _ -> pure "<unknown>"

countRows :: Connection -> EventTableName -> IO Int64
countRows conn tableName =
    query_ conn ("select count(*) from " <> quoteIdent tableName) >>= \case
        [Only n] -> pure n
        _ -> pure 0

queryBool :: ToRow q => Connection -> PG.Query -> q -> IO Bool
queryBool conn q params =
    query conn q params >>= \case
        [Only b] -> pure b
        _ -> pure False

insertMigrationRow
    :: Connection
    -> EventTableBaseName
    -> EventTableVersion
    -> Maybe MigrationTag
    -> MigrationOrigin
    -> IO ()
insertMigrationRow conn base v t origin =
    void $
        execute
            conn
            ( "insert into "
                <> quoteIdent migrationsTableName
                <> " (base_name, version, tag, origin) values (?, ?, ?, ?)"
            )
            (base, v, t, migrationOriginText origin)

-- | Drop every event table of a base name (all versions in the current schema) together
-- with its rows in the metadata table. Meant for test fixtures and for resetting
-- development databases: dropping only the tables leaves metadata behind, and the next
-- startup then fails with 'CurrentEventTableMissing'.
dropEventTables :: Connection -> EventTableBaseName -> IO ()
dropEventTables conn base = do
    unless (isValidBaseName base) $ throwM $ InvalidEventTableBaseName base
    versions <- existingEventTableVersions conn base
    for_ (reverse versions) $ \v ->
        execute_ conn $ "drop table if exists " <> quoteIdent (eventTableNameFor base v)
    hasMetadata <- tableExists conn migrationsTableName
    when hasMetadata . void $
        execute
            conn
            ("delete from " <> quoteIdent migrationsTableName <> " where base_name = ?")
            (Only base)

--------------------------------------------------------------------------------
-- Startup verification and migration
--------------------------------------------------------------------------------

-- | Verify the 'EventTable' chain against the database and bring the database forward
-- to the chain's current version. Runs in the transaction of the given
-- 'OngoingTransaction', which must be freshly begun (nothing may have been executed in
-- it yet); the metadata table must already exist ('ensureMigrationsTable').
--
-- 1. Serialise migrators of the base name with an advisory lock.
-- 2. Read the metadata rows and the existing tables of the base name; adopt tables that
--    are not recorded (databases from before 0.7.0, or migrations run by 0.6 code).
-- 3. Verify: the tags in code must agree with the recorded tags, the database must not
--    be ahead of the code, its current table must exist, and it must not be behind the
--    'TableName' version. Any disagreement throws 'MigrationError' before anything runs.
-- 4. A database without tables for the base name gets only the current table, with
--    metadata rows for the whole chain; no migration function runs.
-- 5. Otherwise, one step per missing version: lock the previous table against writers,
--    create the new table, run the migration function, retire the previous table and
--    record the step.
runMigrations :: (LogEntry -> IO ()) -> OngoingTransaction -> EventTable -> IO ()
runMigrations logger trans et = do
    -- Every statement below must see what was committed while we waited for the locks,
    -- so pin the isolation level rather than relying on the server default.
    void $ execute_ conn "set transaction isolation level read committed"
    logger $ WaitingForMigrationLock base
    advisoryXactLock conn (migrationLockKey base)
    state <- adoptUnrecordedTables logger conn base =<< readEventTableState conn base
    verifyEventTableState conn et state
    case eventTableStateCurrent state of
        Nothing -> bootstrap
        Just current ->
            for_ (filter (\(v, _, _) -> v > current) steps) $
                migrateStep current (state ^. #existingVersions)
  where
    conn :: Connection
    conn = trans ^. field @"connectionResource" . field @"resource"

    (base, baseVersion) = eventTableBase et
    steps = eventTableSteps et
    final = eventTableVersion et

    -- Fresh database: create only the current table and record the whole chain, so that
    -- the history stays checkable after the migrations are deleted from code.
    bootstrap :: IO ()
    bootstrap = do
        void $ createEventTable' conn (eventTableNameFor base final)
        insertMigrationRow conn base baseVersion Nothing OriginBootstrap
        for_ steps $ \(v, t, _) -> insertMigrationRow conn base v (Just t) OriginBootstrap
        logger $ EventTableBootstrapped (eventTableNameFor base final) (length steps)

    migrateStep
        :: EventTableVersion
        -> [EventTableVersion]
        -> (EventTableVersion, MigrationTag, EventMigration)
        -> IO ()
    migrateStep current existingVersions (v, t, mig) = do
        let prevName = eventTableNameFor base (v - 1)
            newName = eventTableNameFor base v
        -- The pre-0.7 migration lock (also the NoIndex writer lock), kept for one
        -- transition release so that concurrent 0.6 migrators still exclude us. Taken
        -- before the table lock: a 0.6 migrator retires the previous table with a
        -- statement that conflicts with the table lock, so the other order can deadlock.
        advisoryXactLock conn (writerLockKey prevName NoIndex)
        -- Blocks every insert into the previous table (indexed writers included) until we
        -- commit, while reads keep working. Together with read committed isolation, the
        -- copy below therefore sees every event that will ever be in the table.
        void . execute_ conn $ "lock table " <> quoteIdent prevName <> " in exclusive mode"
        alreadyMigrated <- tableExists conn newName
        if alreadyMigrated
            then
                -- A 0.6 migrator ran this step while we waited for its lock. The tables
                -- below the new one are the ones found at startup plus those the earlier
                -- steps of this run produced.
                adoptEventTable logger conn base (existingVersions <> [current + 1 .. v - 1]) v
            else do
                t0 <- getCurrentTime
                void $ createEventTable' conn newName
                mig prevName newName conn
                retireTable conn prevName
                insertMigrationRow conn base v (Just t) OriginMigration
                t1 <- getCurrentTime
                logger $ EventTableMigrationDuration (diffUTCTime t1 t0) newName

-- | Record tables that exist without a metadata row. Without any rows (a database from
-- before 0.7.0) the highest existing version is adopted as current. With rows, tables
-- above the recorded maximum are adopted as long as they form a contiguous run from the
-- maximum (a 0.6 instance ran migrations without recording them); a table further up
-- is a stray and throws 'UnrecordedEventTable'. Gaps below the current version are
-- ignored: retired tables may have been dropped to free space.
adoptUnrecordedTables
    :: (LogEntry -> IO ())
    -> Connection
    -> EventTableBaseName
    -> EventTableState
    -> IO EventTableState
adoptUnrecordedTables logger conn base st = do
    toAdopt <- either throwM pure adoptable
    for_ toAdopt $ adoptEventTable logger conn base existing
    pure $ st & #recorded <>~ [MigrationRow v Nothing OriginAdopted | v <- toAdopt]
  where
    existing = st ^. #existingVersions
    adoptable :: Either MigrationError [EventTableVersion]
    adoptable = case (eventTableStateCurrent st, existing) of
        (_, []) -> Right []
        (Nothing, versions) -> Right [maximum versions]
        (Just recordedMax, versions) ->
            let above = filter (> recordedMax) versions
                run = map snd . takeWhile (uncurry (==)) $ zip [recordedMax + 1 ..] above
             in case filter (`notElem` run) above of
                    [] -> Right run
                    stray : _ ->
                        Left . UnrecordedEventTable base (eventTableNameFor base stray) $
                            "event table versions must be contiguous, but "
                                <> eventTableNameFor base (stray - 1)
                                <> " does not exist"

-- | Record an existing table as the (new) current version with unknown tag, after checking
-- that it can be the product of a migration: it has the event table columns, and every
-- lower table that still exists has been retired (only the current table accepts writes).
adoptEventTable
    :: (LogEntry -> IO ())
    -> Connection
    -> EventTableBaseName
    -> [EventTableVersion]
    -- ^ Existing versions of the base name
    -> EventTableVersion
    -> IO ()
adoptEventTable logger conn base existingVersions v = do
    let name = eventTableNameFor base v
    columns <- tableColumns conn name
    case filter (`notElem` columns) eventTableColumns of
        [] -> pure ()
        missing ->
            throwM . UnrecordedEventTable base name $
                "it lacks the event table column(s) " <> intercalate ", " missing
    for_ (filter (< v) existingVersions) $ \lower -> do
        let lowerName = eventTableNameFor base lower
        retired <- isRetired conn lowerName
        unless retired . throwM . UnrecordedEventTable base name $
            lowerName
                <> " still accepts writes (it has not been retired), so "
                <> name
                <> " cannot be the result of a migration"
    insertMigrationRow conn base v Nothing OriginAdopted
    logger $ EventTableAdopted name

-- | Throw 'MigrationError' if the chain and the database disagree. Nothing is modified.
verifyEventTableState :: Connection -> EventTable -> EventTableState -> IO ()
verifyEventTableState conn et st = do
    unless (null disagreements) . throwM $
        MigrationTagMismatch base codeBase disagreements (findTagShift codeTags recordedTags)
    for_ (eventTableStateCurrent st) $ \current -> do
        when (current > final) . throwM $ DatabaseAheadOfCode base current final
        unless (current `elem` (st ^. #existingVersions)) $ do
            schema <- currentSchema conn
            throwM $ CurrentEventTableMissing base current schema
        when (current < codeBase) $ do
            eventCount <- countRows conn (eventTableNameFor base current)
            throwM $ DatabaseBehindCodeBase base current codeBase eventCount
  where
    (base, codeBase) = eventTableBase et
    final = eventTableVersion et
    codeTags = [(v, t) | (v, t, _) <- eventTableSteps et]
    recordedTags = M.fromList [(v, (t, o)) | MigrationRow v (Just t) o <- st ^. #recorded]
    recordedVersionOfTag = M.fromList [(t, v) | MigrationRow v (Just t) _ <- st ^. #recorded]
    disagreements =
        [ TagDisagreement
            { version = v
            , codeTag = t
            , recordedTag = fst <$> recordedHere
            , recordedOrigin = snd <$> recordedHere
            , codeTagRecordedAt = recordedElsewhere
            }
        | (v, t) <- codeTags
        , let recordedHere = M.lookup v recordedTags
              recordedElsewhere = case M.lookup t recordedVersionOfTag of
                Just w | w /= v -> Just w
                _ -> Nothing
        , (fst <$> recordedHere) /= Just t
        , isJust recordedHere || isJust recordedElsewhere
        ]

-- | The offset (if any, smallest magnitude first) at which the code's tag sequence lines
-- up with the recorded history: at that offset no code tag contradicts a recorded tag
-- and at least one matches. A non-zero result means the 'TableName' version is
-- probably off by that much.
findTagShift
    :: [(EventTableVersion, MigrationTag)]
    -> M.Map EventTableVersion (MigrationTag, MigrationOrigin)
    -> Maybe Int
findTagShift codeTags recordedTags = listToMaybe . sortOn abs $ filter alignsAt candidates
  where
    recordedTag v = fst <$> M.lookup v recordedTags
    codeVersions = map fst codeTags
    recordedVersions = M.keys recordedTags
    candidates
        | null codeVersions || null recordedVersions = []
        | otherwise =
            filter
                (/= 0)
                [ minimum recordedVersions - maximum codeVersions
                .. maximum recordedVersions - minimum codeVersions
                ]
    alignsAt n =
        all (\(v, t) -> maybe True (== t) (recordedTag (v + n))) codeTags
            && any (\(v, t) -> recordedTag (v + n) == Just t) codeTags

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
                WaitingForMigrationLock base ->
                    putStrLn $ "[DomainDriven] waiting for migration lock on " <> base
                EventTableBootstrapped etName skipped ->
                    putStrLn $
                        "[DomainDriven] created event table "
                            <> etName
                            <> " on a fresh database; "
                            <> show skipped
                            <> " migration(s) recorded as applied without running"
                EventTableAdopted etName ->
                    putStrLn $
                        "[DomainDriven] adopted existing event table "
                            <> etName
                            <> ", which was not recorded in domaindriven_migrations (tag unknown)"
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
exclusiveLock (OngoingTransaction connR _ _) etName index =
    -- We use advisory locks in favor of row level locks as we would not have the ability
    -- to lock an index before the first event is written with row level locks.
    advisoryXactLock (Pool.resource connR) (writerLockKey etName index)

-- | Key of the advisory lock serialising writers of one index of one event table.
writerLockKey :: IsPgIndex i => EventTableName -> i -> Int64
writerLockKey etName index = fromIntegral $ hash (etName, index)

-- | Take a transaction-scoped advisory lock, blocking until it is available.
advisoryXactLock :: Connection -> Int64 -> IO ()
advisoryXactLock conn key =
    void (query conn "SELECT pg_advisory_xact_lock(?)" (Only key) :: IO [Only ()])

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
