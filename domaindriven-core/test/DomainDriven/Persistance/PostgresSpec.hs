{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Missing NOINLINE pragma" #-}
module DomainDriven.Persistance.PostgresSpec where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.DeepSeq (NFData (rnf))
import Control.Exception
    ( AsyncException (ThreadKilled)
    , Exception
    , SomeAsyncException
    , bracket
    , bracket_
    , displayException
    , throw
    , throwIO
    )
import Control.Exception qualified as Exception
import Control.Monad
import Data.Aeson
    ( FromJSON (parseJSON)
    , ToJSON
    , Value
    , encode
    , object
    , withObject
    , (.:)
    , (.=)
    )
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable
import Data.Maybe (fromMaybe)
import Data.HashMap.Strict qualified as HM
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Data.List qualified as L
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time
import Data.Traversable
import Data.UUID (UUID, nil)
import Data.UUID.V4 qualified as V4
import Database.PostgreSQL.Simple
import DomainDriven.Persistance.Class
import DomainDriven.Persistance.Postgres
import DomainDriven.Persistance.Postgres.Internal
    ( PostgresEventTrans (transaction)
    , createEventTable'
    , existingEventTableVersions
    , getCurrentState
    , migrationLock
    , parseEventRows
    , publishNumberedModel
    , queryHasEventsAfter
    , queryEvents
    , retireTable
    , runMigrations
    , withIOTrans
    , writeEvents
    )
import DomainDriven.Persistance.Postgres.Migration
import DomainDriven.Persistance.Postgres.Types
    ( EventNumber (..)
    , EventRowOut (..)
    , NumberedModel (..)
    , PersistanceError (..)
    , quoteIdent
    )
import GHC.Generics (Generic)
import GHC.IO.Unsafe (unsafePerformIO)
import System.Environment (lookupEnv)
import System.Timeout (timeout)
import Streamly.Data.Stream.Prelude qualified as Stream
import Test.Hspec
import UnliftIO
    ( TVar
    , async
    , atomically
    , checkSTM
    , concurrently
    , forConcurrently
    , modifyTVar
    , newTVarIO
    , readTVar
    , readTVarIO
    , try
    , wait
    , withAsync
    )
import UnliftIO.Pool
import Prelude

testEventsBase :: EventTableBaseName
testEventsBase = "test_events"

-- | Two no-op migrations on top of the base version. On a fresh database only the
-- current table (test_events_v3) is created and the migrations never run.
eventTable :: EventTable
eventTable =
    MigrateTo 3 (\_ _ _ -> pure ())
        . MigrateTo 2 (\_ _ _ -> pure ())
        $ TableName testEventsBase 1

eventTable2 :: EventTable
eventTable2 = MigrateTo 4 copyMigration eventTable

lockEventTable1 :: EventTable
lockEventTable1 = TableName "test_lock_events_1" 1

lockEventTable2 :: EventTable
lockEventTable2 = TableName "test_lock_events_2" 1

spec :: Spec
spec = do
    parallelParsingSpec
    aroundAll (setupPersistance noHook) streamingSpec
    aroundAll (setupPersistance noHook) $ do
        writeEventsSpec
        queryEventsSpec
        migrationSpec -- make sure migrationSpec is run last!
    processedEvents <- runIO $ newTVarIO (Set.empty :: Set UUID)
    hookDone <- runIO newChan
    let postHook
            :: PostgresEvent NoIndex TestModel TestEvent
            -> NoIndex
            -> TestModel
            -> [Stored TestEvent]
            -> IO ()
        postHook p index m evs = do
            atomically $
                modifyTVar processedEvents (<> Set.fromList (fmap storedUUID evs))
            when (m < 0) (void $ runCmd p index $ \_ -> pure (id, [Reset]))
            writeChan hookDone ()
     in around (setupPersistance postHook) (postHookSpec hookDone processedEvents)

    around (setupPersistance noHook) migrationConcurrencySpec
    around (setupPersistance noHook) transactionSpec
    around (setupPersistance noHook) loggingSpec
    around setupPersistanceIndexed indexedSpec
    around setupTableScopedLocks tableScopedLockSpec
    cacheSpec
    around withTestPool versionedMigrationsSpec

type TestModel = Int

data TestEvent
    = AddOne
    | SubtractOne
    | Reset
    deriving (Show, Eq, Generic, FromJSON, ToJSON, NFData)

data ProbeFailure = ForceProbeFailure | MigrationFailure
    deriving stock (Eq, Show)
    deriving anyclass (Exception)

data ForceProbeEvent = ForceProbeEvent ~String

instance FromJSON ForceProbeEvent where
    parseJSON = withObject "ForceProbeEvent" $ \o -> do
        shouldThrowOnForce <- o .: "shouldThrowOnForce"
        let payload =
                if shouldThrowOnForce
                    then throw ForceProbeFailure
                    else "ok"
        pure (ForceProbeEvent payload)

instance NFData ForceProbeEvent where
    rnf (ForceProbeEvent payload) = rnf payload

applyTestEvent :: TestModel -> Stored TestEvent -> TestModel
applyTestEvent m ev = case storedEvent ev of
    AddOne -> m + 1
    SubtractOne -> m - 1
    Reset -> 0

noHook
    :: PostgresEvent NoIndex TestModel TestEvent
    -> NoIndex
    -> TestModel
    -> [Stored TestEvent]
    -> IO ()
noHook _ _ _ _ = pure ()

setupPersistance
    :: ( PostgresEvent NoIndex TestModel TestEvent
         -> NoIndex
         -> TestModel
         -> [Stored TestEvent]
         -> IO ()
       )
    -> ((PostgresEvent NoIndex TestModel TestEvent, Pool Connection) -> IO ())
    -> IO ()
setupPersistance postHook test = do
    withTestConn (`dropEventTables` testEventsBase)
    pool <- simplePool mkTestConn
    p <- postgresWriteModel pool eventTable applyTestEvent 0
    test
        ( p
            { chunkSize = 2
            , parseConcurrency = 2
            , logger = const $ pure () -- putStrLn . ("[DomainDriven] " <>) . show
            , updateHook = postHook
            }
        , pool
        )

setupPersistanceIndexed
    :: ((PostgresEvent Indexed TestModel TestEvent, Pool Connection) -> IO ())
    -> IO ()
setupPersistanceIndexed test = do
    withTestConn (`dropEventTables` testEventsBase)
    -- One stripe makes concurrent tests contend for the same pool.
    pool <- simplePool mkTestConn
    p <- postgresWriteModel pool eventTable applyTestEvent 0
    test (p{chunkSize = 2, parseConcurrency = 2}, pool)

setupTableScopedLocks
    :: ( ( PostgresEvent NoIndex TestModel TestEvent
         , PostgresEvent NoIndex TestModel TestEvent
         )
         -> IO ()
       )
    -> IO ()
setupTableScopedLocks test =
    bracket (simplePool mkLockTimeoutConn) destroyAllResources $ \pool ->
        bracket_ cleanupTables cleanupTables $ do
            p1 <- postgresWriteModel pool lockEventTable1 applyTestEvent 0
            p2 <- postgresWriteModel pool lockEventTable2 applyTestEvent 0
            test (p1, p2)
  where
    mkLockTimeoutConn :: IO Connection
    mkLockTimeoutConn = do
        conn <- mkTestConn
        void $ execute_ conn "set lock_timeout = '1s'"
        pure conn

    cleanupTables :: IO ()
    cleanupTables = withTestConn $ \conn ->
        traverse_
            (dropEventTables conn)
            ["test_lock_events_1", "test_lock_events_2"]

mkTestConn :: IO Connection
mkTestConn = connect =<< testConnectInfo

-- Use libpq environment settings with CI-compatible defaults.
testConnectInfo :: IO ConnectInfo
testConnectInfo = do
    let setting :: String -> String -> IO String
        setting name fallback = fromMaybe fallback <$> lookupEnv name
    ConnectInfo
        <$> setting "PGHOST" "localhost"
        <*> (maybe 5432 read <$> lookupEnv "PGPORT")
        <*> setting "PGUSER" "postgres"
        <*> setting "PGPASSWORD" "postgres"
        <*> setting "PGDATABASE" "domaindriven"

withTestConn :: (Connection -> IO a) -> IO a
withTestConn = bracket mkTestConn close

withTestPool :: (Pool Connection -> IO ()) -> IO ()
withTestPool = bracket (simplePool mkTestConn) destroyAllResources

-- | Drops every event table of a base name in its first schema on the search path.
dropEventTables :: Connection -> EventTableBaseName -> IO ()
dropEventTables conn base = do
    versions <- existingEventTableVersions conn base
    for_ versions $ \v ->
        execute_ conn $ "drop table if exists " <> quoteIdent (eventTableNameFor base v)

-- | Runs the action with an empty schema of the given name, dropping it afterwards.
withSchema :: String -> IO a -> IO a
withSchema schema = bracket_ (dropSchema >> createSchema) dropSchema
  where
    dropSchema :: IO ()
    dropSchema = withTestConn $ \conn ->
        void . execute_ conn $ "drop schema if exists " <> quoteIdent schema <> " cascade"

    createSchema :: IO ()
    createSchema = withTestConn $ \conn ->
        void . execute_ conn $ "create schema " <> quoteIdent schema

-- | A pool whose connections start with the given statement, for example a @set@.
withPoolRunning :: Query -> (Pool Connection -> IO a) -> IO a
withPoolRunning statement = bracket (simplePool mkConn) destroyAllResources
  where
    mkConn :: IO Connection
    mkConn = do
        conn <- mkTestConn
        void $ execute_ conn statement
        pure conn

isRetired :: Connection -> EventTableName -> IO Bool
isRetired conn tableName = do
    [Only retired] <-
        query
            conn
            "select exists (select 1 from pg_trigger \
            \where tgrelid = to_regclass(?) and tgname = 'retired')"
            (Only tableName)
    pure retired

countRows :: Connection -> EventTableName -> IO Int64
countRows conn tableName = do
    [Only rowCount] <- query_ conn $ "select count(*) from " <> quoteIdent tableName
    pure rowCount

writeEventsSpec :: SpecWith (PostgresEvent NoIndex TestModel TestEvent, Pool Connection)
writeEventsSpec = describe "queryEvents" $ do
    let ev1 :: Stored TestEvent
        ev1 =
            Stored
                { storedEvent = AddOne
                , storedTimestamp = UTCTime (fromGregorian 2020 10 15) 0
                , storedUUID = nil
                }

    it "Can write event to database" $ \(_p, pool) -> withResource pool $ \conn -> do
        i <- writeEvents conn (getEventTableName eventTable) NoIndex [ev1]
        i `shouldBe` 1

    it "Writing the same event again fails" $ \(_p, pool) -> withResource pool $ \conn -> do
        writeEvents conn (getEventTableName eventTable) NoIndex [ev1]
            `shouldThrow` (== FatalError)
                . sqlExecStatus

    it "Writing multiple events at once works" $ \(p, pool) -> do
        let evs =
                [ AddOne
                , SubtractOne
                ]
        storedEvs <-
            traverse
                (\e -> Stored e (UTCTime (fromGregorian 2020 10 15) 10) <$> mkId)
                evs
        _ <- withResource pool $ \conn ->
            writeEvents conn (getEventTableName eventTable) NoIndex storedEvs
        evs' <- getEventList p NoIndex
        drop (length evs' - 2) (fmap storedEvent evs') `shouldBe` evs

    it "returns the watermark from the inserted batch" $ \(_p, pool) ->
        withResource pool $ \conn -> do
            unrelatedId <- mkId
            batchId <- mkId
            let timestamp = UTCTime (fromGregorian 2020 10 15) 10
                unrelatedEventNumber = 1000000000000 :: Int64
            void $
                execute
                    conn
                    ( "insert into "
                        <> quoteIdent (getEventTableName eventTable)
                        <> " (id, index, event_number, timestamp, event) \
                           \overriding system value values (?, ?, ?, ?, ?)"
                    )
                    ( unrelatedId
                    , toPgIndex NoIndex
                    , unrelatedEventNumber
                    , timestamp
                    , encode AddOne
                    )
            watermark <-
                writeEvents
                    conn
                    (getEventTableName eventTable)
                    NoIndex
                    [Stored AddOne timestamp batchId]
            [Only batchEventNumber] <-
                query
                    conn
                    ( "select event_number from "
                        <> quoteIdent (getEventTableName eventTable)
                        <> " where id = ?"
                    )
                    (Only batchId)
            watermark `shouldBe` EventNumber batchEventNumber
            batchEventNumber `shouldSatisfy` (< unrelatedEventNumber)

parallelParsingSpec :: Spec
parallelParsingSpec = describe "parseEventRows" $ do
    -- A small chunk size so that, with > parseTaskSize rows, the input is split
    -- into many concurrent tasks (taskSize = chunkSize `div` workers). With
    -- workers = 4 this yields ~64-row tasks, so these specs genuinely exercise
    -- parallel parsing rather than collapsing to a single task.
    let testChunkSize = 256 :: Int
        eventCount = 1000 :: Int

    it "preserves event order when parsing in parallel" $ do
        let ts = UTCTime (fromGregorian 2020 10 15) 0
            events = take eventCount $ cycle [AddOne, SubtractOne, Reset]
            rows =
                zipWith
                    ( \eventNumber event ->
                        EventRowOut nil (EventNumber eventNumber) ts (encodeStrict event)
                    )
                    [1 ..]
                    events

        serial <- parseEventRows @TestEvent 1 testChunkSize rows
        parsedInParallel <- parseEventRows @TestEvent 4 testChunkSize rows

        parsedInParallel `shouldBe` serial
        fmap snd parsedInParallel `shouldBe` fmap (EventNumber . fromIntegral) [1 .. eventCount]
        fmap (storedEvent . fst) parsedInParallel `shouldBe` events

    it "throws the first parse error by input order" $ do
        firstBadUuid <- V4.nextRandom
        secondBadUuid <- V4.nextRandom
        let ts = UTCTime (fromGregorian 2020 10 15) 0
            invalidEvent = encodeStrict $ object ["invalid" .= (1 :: Int)]
            -- Two bad rows in different concurrent tasks (positions 10 and 600,
            -- taskSize ~64); the error from the earlier input position must win.
            rows =
                [ if i == 10
                    then EventRowOut firstBadUuid (EventNumber i) ts invalidEvent
                    else
                        if i == 600
                            then EventRowOut secondBadUuid (EventNumber i) ts invalidEvent
                            else EventRowOut nil (EventNumber i) ts (encodeStrict AddOne)
                | i <- [1 .. fromIntegral eventCount]
                ]

        parseEventRows @TestEvent 4 testChunkSize rows `shouldThrow` \case
            EncodingError msg -> show firstBadUuid `L.isInfixOf` msg
            ValueError _ -> False

    it "fully forces parsed events before returning" $ do
        let ts = UTCTime (fromGregorian 2020 10 15) 0
            row =
                EventRowOut
                    nil
                    1
                    ts
                    ( encodeStrict $
                        object ["shouldThrowOnForce" .= True]
                    )

        parseEventRows @ForceProbeEvent 1 testChunkSize [row] `shouldThrow` (== ForceProbeFailure)

encodeStrict :: ToJSON a => a -> ByteString
encodeStrict = LBS.toStrict . encode

indexedSpec :: SpecWith (PostgresEvent Indexed TestModel TestEvent, Pool Connection)
indexedSpec = describe "Indexed models" $ do
    it "Models with different indices are updated separately" $ \(p, pool) -> do
        let evs1 = [AddOne, SubtractOne, AddOne]
            evs2 = [AddOne, AddOne, AddOne]

        storedEvs1 <-
            traverse
                (\e -> Stored e (UTCTime (fromGregorian 2020 10 15) 10) <$> mkId)
                evs1
        storedEvs2 <-
            traverse
                (\e -> Stored e (UTCTime (fromGregorian 2020 10 15) 10) <$> mkId)
                evs2
        _ <- withResource pool $ \conn ->
            writeEvents conn (getEventTableName eventTable) (Indexed "1") storedEvs1
        _ <- withResource pool $ \conn ->
            writeEvents conn (getEventTableName eventTable) (Indexed "2") storedEvs2
        m1 <- getModel p (Indexed "1")
        m2 <- getModel p (Indexed "2")
        m1 `shouldBe` 1
        m2 `shouldBe` 3

    it "does not refresh one index after another index changes" $ \(p, pool) -> do
        let indexA = Indexed "a"
            indexB = Indexed "b"
            stored event = Stored event (UTCTime (fromGregorian 2020 10 15) 10) <$> mkId
        eventA <- stored AddOne
        withResource pool $ \conn ->
            void $ writeEvents conn (getEventTableName eventTable) indexA [eventA]
        getModel p indexA `shouldReturn` 1

        logVar <- newTVarIO []
        let logged = p{logger = \entry -> atomically $ modifyTVar logVar (entry :)}
        eventB <- stored AddOne
        withResource pool $ \conn ->
            void $ writeEvents conn (getEventTableName eventTable) indexB [eventB]
        getModel logged indexA `shouldReturn` 1
        logsAfterIndexB <- readTVarIO logVar
        logsAfterIndexB `shouldSatisfy` (not . null)
        logsAfterIndexB `shouldSatisfy` all \case
            EventTableLockDuration{} -> False
            DbTransactionDuration{} -> False
            EventTableMigrationDuration{} -> True
            WaitForConnectionDuration{} -> True
            WaitingForMigrationLock{} -> True

        nextEventA <- stored AddOne
        withResource pool $ \conn ->
            void $ writeEvents conn (getEventTableName eventTable) indexA [nextEventA]
        getModel logged indexA `shouldReturn` 2
        logs <- readTVarIO logVar
        logs `shouldSatisfy` any \case
            EventTableLockDuration{} -> True
            DbTransactionDuration{} -> False
            EventTableMigrationDuration{} -> False
            WaitForConnectionDuration{} -> False
            WaitingForMigrationLock{} -> False

    it "retains the zero watermark for an empty transaction" $ \(p, pool) -> do
        let index = Indexed "empty"
        runCmd p index (\_ -> pure (id, [])) `shouldReturn` 0
        NumberedModel _ cachedEventNumber <- getCurrentState p index
        cachedEventNumber `shouldBe` 0

        writer <- postgresWriteModel pool eventTable applyTestEvent 0
        runCmd writer index (\_ -> pure (id, [AddOne])) `shouldReturn` 1
        getModel p index `shouldReturn` 1

    it "uses the compound index for freshness checks" $ \(_p, pool) ->
        withResource pool $ \conn -> do
            let tableName = getEventTableName eventTable
                targetIndex = Indexed "target"
            void $
                execute_ conn $
                    "insert into "
                        <> quoteIdent tableName
                        <> " (id, index, timestamp, event) \
                           \select md5(i::text)::uuid, 'bulk', now(), '\"AddOne\"'::jsonb \
                           \from generate_series(1, 500000) as i"
            targetEvent <-
                Stored AddOne (UTCTime (fromGregorian 2020 10 15) 10) <$> mkId
            void $ writeEvents conn tableName targetIndex [targetEvent]
            void $ execute_ conn $ "analyze " <> quoteIdent tableName
            planRows <-
                query
                    conn
                    ( "explain (analyze, buffers) select exists (select 1 from "
                        <> quoteIdent tableName
                        <> " where index = ? and event_number > ?)"
                    )
                    (toPgIndex targetIndex, 0 :: Int64)
            let plan = unlines (fmap fromOnly planRows)
            plan `shouldSatisfy` \queryPlan ->
                "Index Only Scan" `L.isInfixOf` queryPlan
                    || "Index Scan" `L.isInfixOf` queryPlan
            plan `shouldNotContain` "Seq Scan"
            plan `shouldNotContain` "Aggregate"
            queryHasEventsAfter conn tableName targetIndex 0 `shouldReturn` True

    it "round-trips indices containing SQL syntax through every read path" $ \(p, pool) -> do
        let tableName = getEventTableName eventTable
            indices =
                [ Indexed "it's"
                , Indexed "x' or '1'='1"
                , Indexed ("x'; drop table " <> T.pack (show tableName) <> "; --")
                , Indexed "\"double\" \\ backslash"
                , Indexed "ünïcödé ✓"
                , Indexed "a?b"
                ]
        for_ indices $ \index ->
            runCmd p index (\_ -> pure (id, [AddOne])) `shouldReturn` 1
        reader <- postgresWriteModel pool eventTable applyTestEvent 0
        for_ indices $ \index -> do
            getModel reader index `shouldReturn` 1
            fmap storedEvent <$> getEventList reader index `shouldReturn` [AddOne]
            fmap storedEvent <$> Stream.toList (getEventStream reader index) `shouldReturn` [AddOne]
        getModel reader (Indexed "x") `shouldReturn` 0
        let rejectsNul :: PersistanceError -> Bool
            rejectsNul = \case
                ValueError _ -> True
                EncodingError _ -> False
        runCmd p (Indexed "a\0b") (\_ -> pure (id, [AddOne])) `shouldThrow` rejectsNul
        getModel reader (Indexed "a\0b") `shouldThrow` rejectsNul
        withResource pool $ \conn -> do
            [Only indexCount] <- query_ conn $ "select count(distinct index) from " <> quoteIdent tableName
            indexCount `shouldBe` (fromIntegral (length indices) :: Int64)

    it "creates the event table and its index idempotently, also for long names" $ \(_p, pool) -> do
        -- Long enough for Postgres to truncate the generated index name.
        let tableName = "test_events_v1_with_a_rather_long_table_name"
        withResource pool $ \conn -> do
            void . execute_ conn $ "drop table if exists " <> quoteIdent tableName
            void . execute_ conn $
                "create table "
                    <> quoteIdent tableName
                    <> " (id uuid primary key, index varchar not null, \
                       \event_number bigint not null generated always as identity, \
                       \timestamp timestamptz not null default now(), event jsonb not null)"
            void . execute_ conn $
                "create index on " <> quoteIdent tableName <> " (index, event_number)"
        replicateM_ 2 $
            void
                ( postgresWriteModelNoMigration pool tableName applyTestEvent 0
                    :: IO (PostgresEvent Indexed TestModel TestEvent)
                )
        withResource pool $ \conn -> do
            [Only indexCount] <-
                query conn "select count(*) from pg_indexes where tablename = ?" (Only tableName)
            -- Primary key plus (index, event_number).
            indexCount `shouldBe` (2 :: Int64)

    it "hands the hook and readers the same stored events" $ \(p, pool) -> do
        let index = Indexed "timestamps"
        hookEvents <- newEmptyMVar
        let observed = p{updateHook = \_ _ _ evs -> putMVar hookEvents evs}
        runCmd observed index (\_ -> pure (id, [AddOne, SubtractOne, AddOne])) `shouldReturn` 1
        hooked <- takeMVar hookEvents
        fmap storedEvent hooked `shouldBe` [AddOne, SubtractOne, AddOne]
        reader <- postgresWriteModel pool eventTable applyTestEvent 0
        getEventList reader index `shouldReturn` hooked
        getEventList p index `shouldReturn` hooked

    it "rejects invalid raw table names before acquiring a connection" $ \(p, _pool) -> do
        let invalidNames :: [(EventTableName, MigrationError)]
            invalidNames =
                [(name, InvalidEventTableName name) | name <- ["", "bad;name", "bad\"name", "bad\0name", replicate 32 'é']]
                    <> [(name, EventTableNameTooLong name) | name <- fmap (replicate 63 'a' <>) ["x", "y"]]
        bracket (simplePool (fail "Unexpected connection acquisition")) destroyAllResources $ \pool ->
            for_ invalidNames $ \(tableName, expected) -> do
                ( postgresWriteModelNoMigration pool tableName applyTestEvent 0
                    :: IO (PostgresEvent Indexed TestModel TestEvent)
                    ) `shouldThrow` (== expected)
                let alias :: PostgresEvent Indexed TestModel TestEvent
                    alias = p{connectionPool = pool, eventTableName = tableName}
                runCmd alias (Indexed "a") (\_ -> pure (id, [AddOne]))
                    `shouldThrow` (== expected)
                getModel alias (Indexed "a") `shouldThrow` (== expected)
                getEventList alias (Indexed "a") `shouldThrow` (== expected)
                Stream.toList (getEventStream alias (Indexed "a")) `shouldThrow` (== expected)

    it "accepts a 63-character raw table name without aliasing its suffixes" $ \(_p, pool) -> do
        let tableName :: EventTableName
            tableName = "test_events_v" <> replicate 50 'a'
        withResource pool $ \conn ->
            void . execute_ conn $ "drop table if exists " <> quoteIdent tableName
        backend <- postgresWriteModelNoMigration pool tableName applyTestEvent 0
        runCmd backend (Indexed "a") (\_ -> pure (id, [AddOne])) `shouldReturn` 1
        for_ ["x", "y"] $ \suffix -> do
            ( postgresWriteModelNoMigration pool (tableName <> suffix) applyTestEvent 0
                :: IO (PostgresEvent Indexed TestModel TestEvent)
                ) `shouldThrow` (== EventTableNameTooLong (tableName <> suffix))
            let alias :: PostgresEvent Indexed TestModel TestEvent
                alias = backend{eventTableName = tableName <> suffix}
            runCmd alias (Indexed "a") (\_ -> pure (id, [AddOne]))
                `shouldThrow` (== EventTableNameTooLong (tableName <> suffix))
            getModel alias (Indexed "a") `shouldThrow` (== EventTableNameTooLong (tableName <> suffix))
            getEventList alias (Indexed "a") `shouldThrow` (== EventTableNameTooLong (tableName <> suffix))
            Stream.toList (getEventStream alias (Indexed "a"))
                `shouldThrow` (== EventTableNameTooLong (tableName <> suffix))
        reader <- postgresWriteModelNoMigration pool tableName applyTestEvent 0
        getModel reader (Indexed "a") `shouldReturn` 1
        fmap storedEvent <$> getEventList reader (Indexed "a") `shouldReturn` [AddOne]

    it "blocks indexed writers while their table is migrated" $ \(p, pool) -> do
        runCmd p (Indexed "a") (\_ -> pure (id, [AddOne])) `shouldReturn` 1
        copyStarted <- newEmptyMVar
        let waitForBlockedWriter :: Connection -> EventTableName -> IO ()
            waitForBlockedWriter conn tableName = do
                -- The late writer queues on the shared table key, before INSERT.
                result <-
                    query_
                        conn
                        "select exists (\
                        \select 1 from pg_locks \
                        \where locktype = 'advisory' \
                        \and mode = 'ShareLock' and not granted)"
                case result of
                    [Only True] -> pure ()
                    [Only False] -> waitForBlockedWriter conn tableName
                    unexpected -> expectationFailure $ "Unexpected lock query result: " <> show unexpected

            migrated :: EventTable
            migrated =
                MigrateTo
                    4
                    ( \prev next conn -> do
                        putMVar copyStarted ()
                        waitForBlockedWriter conn prev
                        migrate1to1 @Indexed @Value conn prev next id
                    )
                    eventTable
        outcome <-
            timeout 5000000 $
                concurrently
                    ( do
                        takeMVar copyStarted
                        try @IO @SqlError $ runCmd p (Indexed "b") (\_ -> pure (id, [AddOne]))
                    )
                    ( void
                        ( postgresWriteModel pool migrated applyTestEvent 0
                            :: IO (PostgresEvent Indexed TestModel TestEvent)
                        )
                    )
        case outcome of
            Nothing -> expectationFailure "Timed out waiting for the writer to block during migration"
            Just (writer, _) -> do
                writer `shouldSatisfy` \case
                    Left err -> sqlErrorMsg err == "Event table has been retired."
                    Right _ -> False
                withResource pool $ \conn -> do
                    [Only oldCount] <- query_ conn $ "select count(*) from " <> quoteIdent (getEventTableName eventTable)
                    [Only newCount] <- query_ conn $ "select count(*) from " <> quoteIdent (getEventTableName migrated)
                    (oldCount :: Int64, newCount :: Int64) `shouldBe` (1, 1)

    it "runs a migration once when two instances start concurrently" $ \(p, pool) -> do
        runCmd p (Indexed "a") (\_ -> pure (id, [AddOne, AddOne])) `shouldReturn` 2
        let migrated :: EventTable
            migrated = MigrateTo 4 (\prev next conn -> migrate1to1 @Indexed @Value conn prev next id) eventTable
            start :: IO (PostgresEvent Indexed TestModel TestEvent)
            start = postgresWriteModel pool migrated applyTestEvent 0
        (p1, p2) <- concurrently start start
        getModel p1 (Indexed "a") `shouldReturn` 2
        getModel p2 (Indexed "a") `shouldReturn` 2
        withResource pool $ \conn -> do
            [Only newCount] <- query_ conn $ "select count(*) from " <> quoteIdent (getEventTableName migrated)
            newCount `shouldBe` (2 :: Int64)

    it "waits for in-flight commands and copies their events" $ \(p, pool) -> do
        runCmd p (Indexed "a") (\_ -> pure (id, [AddOne])) `shouldReturn` 1
        commandStarted <- newEmptyMVar
        releaseCommand <- newEmptyMVar
        let waitForQueuedMigrator :: Connection -> IO ()
            waitForQueuedMigrator conn = do
                result <-
                    query_
                        conn
                        "select exists (\
                        \select 1 from pg_locks \
                        \where locktype = 'advisory' \
                        \and mode = 'ExclusiveLock' and not granted)"
                case result of
                    [Only True] -> pure ()
                    [Only False] -> waitForQueuedMigrator conn
                    unexpected -> expectationFailure $ "Unexpected lock query result: " <> show unexpected

            migrated :: EventTable
            migrated =
                MigrateTo 4 (\prev next conn -> migrate1to1 @Indexed @Value conn prev next id) eventTable

            inFlightCommand :: TestModel -> IO (TestModel -> TestModel, [TestEvent])
            inFlightCommand _ = do
                putMVar commandStarted ()
                takeMVar releaseCommand
                pure (id, [AddOne])
        outcome <- timeout 10000000 $ do
            (writer, _) <-
                concurrently
                    (runCmd p (Indexed "b") inFlightCommand)
                    ( do
                        takeMVar commandStarted
                        migrator <-
                            async
                                ( void
                                    ( postgresWriteModel pool migrated applyTestEvent 0
                                        :: IO (PostgresEvent Indexed TestModel TestEvent)
                                    )
                                )
                        withResource pool waitForQueuedMigrator
                        putMVar releaseCommand ()
                        wait migrator
                    )
            pure writer
        case outcome of
            Nothing ->
                expectationFailure "Timed out waiting for the migration to drain the in-flight command"
            Just writer -> writer `shouldBe` 1
        withResource pool $ \conn -> do
            [Only oldCount] <-
                query_ conn $ "select count(*) from " <> quoteIdent (getEventTableName eventTable)
            [Only newCount] <-
                query_ conn $ "select count(*) from " <> quoteIdent (getEventTableName migrated)
            (oldCount :: Int64, newCount :: Int64) `shouldBe` (2, 2)

    it "allows a nested command on another index of the same table" $ \(p, _pool) -> do
        outcome <- timeout 5000000 $
            runCmd p (Indexed "outer") $ \_ -> do
                inner <- runCmd p (Indexed "inner") $ \_ -> pure (id, [AddOne, AddOne])
                pure (const inner, [AddOne])
        outcome `shouldBe` Just 2
        getModel p (Indexed "outer") `shouldReturn` 1

    -- A finite lock_timeout on the connection is kept, so the nested command fails
    -- with lock_not_available. With the server default (0, no timeout) the library
    -- applies a transaction-local 60s timeout; rather than wait that out, check that the
    -- nested command is still queued after two seconds and cancel it (query_canceled).
    for_ [("0", "57014", 10000000), ("100ms", "55P03", 2000000) :: (T.Text, ByteString, Int)] $ \(configuredTimeout, expectedState, deadline) ->
        it ("bounds nested commands behind migrations with lock_timeout=" <> T.unpack configuredTimeout) $ \(p, pool) -> do
            runCmd p (Indexed "a") (\_ -> pure (id, [AddOne])) `shouldReturn` 1
            commandStarted <- newEmptyMVar
            migrationPid <- newEmptyMVar
            startNested <- newEmptyMVar
            let mkCommandConn :: IO Connection
                mkCommandConn = do
                    conn <- mkTestConn
                    void (query conn "select set_config('lock_timeout', ?, false)" (Only configuredTimeout) :: IO [Only T.Text])
                    pure conn

                mkMigrationConn :: IO Connection
                mkMigrationConn = do
                    conn <- mkTestConn
                    [Only pid] <- query_ conn "select pg_backend_pid()"
                    putMVar migrationPid (pid :: Int)
                    pure conn

                migrated :: EventTable
                migrated = MigrateTo 4 (\prev next conn -> migrate1to1 @Indexed @Value conn prev next id) eventTable

                waitForQueuedMigrator :: Connection -> Int -> IO ()
                waitForQueuedMigrator conn pid = do
                    result <- query conn
                        "select exists (select 1 from pg_locks where pid = ? \
                        \and locktype = 'advisory' and mode = 'ExclusiveLock' and not granted)"
                        (Only pid)
                    case result of
                        [Only True] -> pure ()
                        [Only False] -> threadDelay 1000 >> waitForQueuedMigrator conn pid
                        unexpected -> expectationFailure $ "Unexpected lock query result: " <> show unexpected

                -- The nested command's shared table-key request queues behind the
                -- migrator's exclusive one. It must still be queued after two seconds.
                cancelQueuedNestedCommand :: Connection -> IO ()
                cancelQueuedNestedCommand conn = do
                    threadDelay 2000000
                    waiting <-
                        query_ conn
                            "select pid from pg_locks where locktype = 'advisory' \
                            \and mode = 'ShareLock' and not granted"
                    case waiting of
                        [Only nestedPid] ->
                            void (query conn "select pg_cancel_backend(?)" (Only (nestedPid :: Int)) :: IO [Only Bool])
                        unexpected ->
                            expectationFailure $
                                "Expected the nested command to still be queued, found: " <> show unexpected

            bracket (simplePool mkCommandConn) destroyAllResources $ \commandPool ->
                bracket (simplePool mkMigrationConn) destroyAllResources $ \migrationPool -> do
                    let backend :: PostgresEvent Indexed TestModel TestEvent
                        backend = p{connectionPool = commandPool}
                        startMigration :: IO (PostgresEvent Indexed TestModel TestEvent)
                        startMigration = postgresWriteModel migrationPool migrated applyTestEvent 0
                    outcome <- timeout deadline $
                        concurrently
                            ( try @IO @SqlError $ runCmd backend (Indexed "outer") $ \_ -> do
                                putMVar commandStarted ()
                                takeMVar startNested
                                inner <- runCmd backend (Indexed "inner") $ \_ -> pure (id, [AddOne])
                                pure (const inner, [AddOne])
                            )
                            ( do
                                takeMVar commandStarted
                                withAsync startMigration $ \migrator -> do
                                    pid <- takeMVar migrationPid
                                    withResource pool $ \conn -> waitForQueuedMigrator conn pid
                                    putMVar startNested ()
                                    when (configuredTimeout == "0") $
                                        withResource pool cancelQueuedNestedCommand
                                    wait migrator
                            )
                    case outcome of
                        Nothing -> expectationFailure "Nested command and migration did not finish before the deadline"
                        Just (writer, migratedBackend) -> do
                            writer `shouldSatisfy` \case
                                Left err -> sqlState err == expectedState
                                Right _ -> False
                            for_ [backend, migratedBackend] $ \reader -> do
                                getModel reader (Indexed "a") `shouldReturn` 1
                                getEventList reader (Indexed "outer") `shouldReturn` []
                                getEventList reader (Indexed "inner") `shouldReturn` []
                            runCmd migratedBackend (Indexed "outer") (\_ -> pure (id, [AddOne])) `shouldReturn` 1
                    withResource commandPool $ \conn ->
                        query_ conn "show lock_timeout" `shouldReturn` [Only configuredTimeout]

    it "Updates to different indices can be done in parallel" $ \(p, _pool) -> do
        let testCmd :: Int -> TestModel -> IO (TestModel -> TestModel, [TestEvent])
            testCmd i _ = do
                threadDelay 100000 -- 0.1s delay
                pure (id, replicate i AddOne)
        t0 <- getCurrentTime
        models <- forConcurrently ([1 .. 20] :: [Int]) $ \i -> do
            let index = Indexed (T.pack $ show i)
            runCmd p index $ testCmd i

        t1 <- getCurrentTime

        models `shouldSatisfy` (== 20) . length
        models `shouldSatisfy` (== [1, 2 .. 20]) . L.sort
        print $ diffUTCTime t1 t0
        diffUTCTime t1 t0 `shouldSatisfy` (> 0.1)
        diffUTCTime t1 t0 `shouldSatisfy` (< 1.9)

    it "Updates to same index are done sequentially" $ \(p, _pool) -> do
        let testCmd :: TestModel -> IO (TestModel -> TestModel, [TestEvent])
            testCmd _ = do
                threadDelay 100000 -- 0.1s delay
                pure (id, [AddOne, AddOne])
        t0 <- getCurrentTime
        models <- forConcurrently ([1 .. 20] :: [Int]) $ \_ -> do
            let index = Indexed "the same"
            runCmd p index testCmd

        t1 <- getCurrentTime

        models `shouldSatisfy` (== 20) . length
        models `shouldSatisfy` (== [2, 4 .. 40]) . L.sort
        print $ diffUTCTime t1 t0
        diffUTCTime t1 t0 `shouldSatisfy` (> 20 * 0.1)

cacheSpec :: Spec
cacheSpec = describe "Postgres model cache" $
    it "does not replace a newer model with an older publication" $ do
        ref <- newIORef HM.empty
        let index = Indexed "monotonic"
        publishNumberedModel ref index (NumberedModel 2 2)
        publishNumberedModel ref index (NumberedModel 1 1)
        cached <- HM.lookup index <$> readIORef ref
        case cached of
            Just (NumberedModel cachedModel cachedEventNumber) -> do
                cachedModel `shouldBe` (2 :: Int)
                cachedEventNumber `shouldBe` 2
            Nothing -> expectationFailure "Expected a cached model"

tableScopedLockSpec
    :: SpecWith
        ( PostgresEvent NoIndex TestModel TestEvent
        , PostgresEvent NoIndex TestModel TestEvent
        )
tableScopedLockSpec = describe "Advisory locks" $ do
    it "Do not block the same index in different event tables" $ \(p1, p2) -> do
        firstCommandStarted <- newChan
        releaseFirstCommand <- newChan

        let firstCommand :: TestModel -> IO (TestModel -> TestModel, [TestEvent])
            firstCommand _ = do
                writeChan firstCommandStarted ()
                readChan releaseFirstCommand
                pure (id, [AddOne])

            runSecondCommand :: IO (Either SqlError TestModel)
            runSecondCommand = do
                readChan firstCommandStarted
                result <- try @IO @SqlError $ runCmd p2 NoIndex $ \_ ->
                    pure (id, [AddOne])
                writeChan releaseFirstCommand ()
                pure result

        (firstResult, secondResult) <-
            concurrently
                (runCmd p1 NoIndex firstCommand)
                runSecondCommand

        firstResult `shouldBe` 1
        case secondResult of
            Right secondModel -> secondModel `shouldBe` 1
            Left err -> expectationFailure $ "Second table lock was blocked: " <> show err

streamingSpec :: SpecWith (PostgresEvent NoIndex TestModel TestEvent, Pool Connection)
streamingSpec = describe "steaming" $ do
    it "getEventList and getEventStream yields the same result" $ \(p, pool) -> do
        storedEvs <- for ([1 .. 10] :: [Int]) $ \i -> do
            Stored AddOne (UTCTime (fromGregorian 2020 10 15) (fromIntegral i)) <$> mkId
        _ <- withResource pool $ \conn ->
            writeEvents conn (getEventTableName eventTable) NoIndex storedEvs
        evList <- getEventList p NoIndex
        evStream <- Stream.toList $ getEventStream p NoIndex
        -- pPrint evList
        evList `shouldSatisfy` (== 10) . length -- must be at least two to verify order
        fmap storedEvent evStream `shouldBe` fmap storedEvent evList
        evStream `shouldBe` evList

queryEventsSpec :: SpecWith (PostgresEvent NoIndex TestModel TestEvent, Pool Connection)
queryEventsSpec = describe "queryEvents" $ do
    it "Can query events" $ \(_p, pool) -> withResource pool $ \conn -> do
        evs <- queryEvents @TestEvent conn (getEventTableName eventTable) NoIndex
        evs `shouldSatisfy` not . null
    it "Events come out in the right order" $ \(_p, pool) -> withResource pool $ \conn -> do
        -- write few more events before
        --
        _ <- do
            id1 <- mkId
            let ev1 = SubtractOne
            _ <-
                writeEvents
                    conn
                    (getEventTableName eventTable)
                    NoIndex
                    [Stored ev1 (UTCTime (fromGregorian 2020 10 20) 1) id1]

            id2 <- mkId
            let ev2 = AddOne
            writeEvents
                conn
                (getEventTableName eventTable)
                NoIndex
                [Stored ev2 (UTCTime (fromGregorian 2020 10 18) 1) id2]

        evs <- queryEvents @TestEvent conn (getEventTableName eventTable) NoIndex
        evs `shouldSatisfy` (> 1) . length
        let event_numbers = fmap snd evs
        event_numbers `shouldSatisfy` (\n -> and $ zipWith (>) (drop 1 n) n)

postHookSpec
    :: Chan ()
    -> TVar (Set UUID)
    -> SpecWith (PostgresEvent NoIndex TestModel TestEvent, Pool Connection)
postHookSpec hookDone processedEvents = describe "updateHook" $ do
    it "Ensure we start with empty TVar" $ \_ -> do
        events <- readTVarIO processedEvents
        events `shouldBe` Set.empty

    it "Post update hook is fired after events are written" $ \(p, _) -> do
        i <- runCmd p NoIndex $ \_ -> do
            pure (id, [AddOne, AddOne, SubtractOne])
        i `shouldBe` 1
        readChan hookDone
        events <- readTVarIO processedEvents
        Set.size events `shouldBe` 3

    it "Hook that resets on negative works" $ \(p, _) -> do
        -- the hook will check if the model is negative and reset it if so
        m <- runCmd p NoIndex $ \_ -> do
            pure (id, [SubtractOne, SubtractOne, SubtractOne])
        m `shouldBe` (-3)
        readChan hookDone
        m' <- getModel p NoIndex
        m' `shouldBe` 0

migrationSpec :: SpecWith (PostgresEvent NoIndex TestModel TestEvent, Pool Connection)
migrationSpec = describe "migrate1to1" $ do
    it "Keeps all events when using `id` to update" $ \(_p, pool) -> do
        evs <- withResource pool $ \conn ->
            queryEvents @TestEvent conn (getEventTableName eventTable) NoIndex
        evs `shouldSatisfy` not . null

        _ <- postgresWriteModel pool eventTable2 applyTestEvent 0
        evs' <- withResource pool $ \conn ->
            queryEvents @TestEvent conn (getEventTableName eventTable2) NoIndex

        fmap fst evs' `shouldBe` fmap fst evs

    it "Can no longer write new events to old table after migration" $ \(_p, pool) -> do
        uuid <- V4.nextRandom
        let ev =
                Stored
                    AddOne
                    (UTCTime (fromGregorian 2020 10 15) 0)
                    uuid
        withResource
            pool
            (\conn -> writeEvents conn (getEventTableName eventTable) NoIndex [ev])
            `shouldThrow` (== FatalError)
                . sqlExecStatus
    it "But can write to the new table" $ \(_p, pool) -> do
        uuid <- V4.nextRandom
        let ev =
                Stored
                    AddOne
                    (UTCTime (fromGregorian 2020 10 15) 0)
                    uuid

        void . withResource pool $ \conn ->
            writeEvents conn (getEventTableName eventTable2) NoIndex [ev]

    it "Broken migration throws and rollbacks transaction" $ \(_, pool) -> do
        let eventTableBroken :: EventTable
            eventTableBroken = MigrateTo 5 (\_ _ _ -> throwIO MigrationFailure) eventTable2

        postgresWriteModel pool eventTableBroken applyTestEvent 0
            `shouldThrow` (== MigrationFailure)

        withTestConn $ \conn ->
            existingEventTableVersions conn testEventsBase `shouldReturn` [3, 4]

    it "migrate1toManyWithState threads state in order and resets it per index" $ \(_p, pool) -> do
        let statefulTable :: EventTable
            statefulTable = TableName "test_events_stateful" 1

            migratedTable :: EventTable
            migratedTable = MigrateTo 2 statefulMigration statefulTable

            eventValue :: String -> Value
            eventValue label = object ["label" .= label]

            storedValue :: Value -> IO (Stored Value)
            storedValue value =
                Stored value (UTCTime (fromGregorian 2020 10 15) 0) <$> mkId

            statefulMigration :: PreviousEventTableName -> EventTableName -> Connection -> IO ()
            statefulMigration prevName name conn =
                migrate1toManyWithState @Indexed @Value @Value @Int
                    conn
                    prevName
                    name
                    ( \state stored ->
                        let state' = state + 1
                            migrated =
                                stored
                                    { storedEvent =
                                        object
                                            [ "sequence" .= state'
                                            , "input" .= storedEvent stored
                                            ]
                                    }
                         in (state', [migrated])
                    )
                    0

        withResource pool (`dropEventTables` "test_events_stateful")
        _ <-
            postgresWriteModelNoMigration
                pool
                (getEventTableName statefulTable)
                (\model _ -> model)
                ()
                :: IO (PostgresEvent Indexed () Value)

        aEvents <- traverse (storedValue . eventValue) ["a1", "a2"]
        bEvents <- traverse (storedValue . eventValue) ["b1"]
        withResource pool $ \conn -> do
            void $
                writeEvents
                    conn
                    (getEventTableName statefulTable)
                    (Indexed "a'1")
                    aEvents
            void $
                writeEvents
                    conn
                    (getEventTableName statefulTable)
                    (Indexed "b")
                    bEvents

        _ <-
            postgresWriteModel
                pool
                migratedTable
                (\model _ -> model)
                ()
                :: IO (PostgresEvent Indexed () Value)

        withResource pool $ \conn -> do
            aMigrated <-
                fmap (storedEvent . fst)
                    <$> queryEvents @Value conn (getEventTableName migratedTable) (Indexed "a'1")
            bMigrated <-
                fmap (storedEvent . fst)
                    <$> queryEvents @Value conn (getEventTableName migratedTable) (Indexed "b")

            aMigrated
                `shouldBe` [ object ["sequence" .= (1 :: Int), "input" .= eventValue "a1"]
                           , object ["sequence" .= (2 :: Int), "input" .= eventValue "a2"]
                           ]
            bMigrated
                `shouldBe` [object ["sequence" .= (1 :: Int), "input" .= eventValue "b1"]]

migrationConcurrencySpec
    :: SpecWith (PostgresEvent NoIndex TestModel TestEvent, Pool Connection)
migrationConcurrencySpec = describe "Event table is locked during migration" $ do
    it "migrate1to1" $ \(m0, pool) -> migrationTest m0 pool mig1to1
    it "migrate1toMany" $ \(m0, pool) -> migrationTest m0 pool mig1toMany
    it "migrate1toManyWithState" $ \(m0, pool) -> migrationTest m0 pool mig1toManyState
  where
    migrationTest
        :: PostgresEvent NoIndex TestModel TestEvent
        -> Pool Connection
        -> EventMigration
        -> IO ()
    migrationTest m0 pool mig = do
        let cmd :: Int -> IO (Int -> Int, [TestEvent])
            cmd _ = pure (id, [AddOne])

        i <- replicateM 5 (runCmd m0 NoIndex cmd)
        length i `shouldBe` 5
        (result, _) <-
            concurrently
                ( do
                    threadDelay 100000 -- sleep a bit and let the migration start
                    try @IO @SqlError $ runCmd m0 NoIndex cmd
                )
                ( postgresWriteModel
                    pool
                    (MigrateTo 5 mig eventTable2)
                    applyTestEvent
                    0
                )
        result `shouldSatisfy` \case
            Right _ -> False
            Left err -> sqlErrorMsg err == "Event table has been retired."

    mig1to1 :: PreviousEventTableName -> EventTableName -> Connection -> IO ()
    mig1to1 prevName name conn = migrate1to1 @NoIndex @Value conn prevName name slowId

    mig1toMany :: PreviousEventTableName -> EventTableName -> Connection -> IO ()
    mig1toMany prevName name conn = migrate1toMany @NoIndex @Value conn prevName name (pure . slowId)

    mig1toManyState :: PreviousEventTableName -> EventTableName -> Connection -> IO ()
    mig1toManyState prevName name conn = do
        putStrLn "mig1toManyState"
        migrate1toManyWithState @NoIndex @Value
            conn
            prevName
            name
            (\s ev -> (s, [slowId ev]))
            ()
        putStrLn "mig1toManyState is done"

transactionSpec :: SpecWith (PostgresEvent NoIndex TestModel TestEvent, Pool Connection)
transactionSpec = describe "Postgres transactions" $ do
    it "rolls back when a logger receives asynchronous cancellation" $ \(p, pool) -> do
        let cancellingLogger = \case
                EventTableLockDuration{} -> throwIO ThreadKilled
                DbTransactionDuration{} -> pure ()
                EventTableMigrationDuration{} -> pure ()
                WaitForConnectionDuration{} -> pure ()
                WaitingForMigrationLock{} -> pure ()
            backendWithCancellingLogger = p{logger = cancellingLogger}
        result <-
            Exception.try @SomeAsyncException $
                runCmd backendWithCancellingLogger NoIndex $ \_ ->
                    pure (id, [AddOne])
        case result of
            Left cancellation -> displayException cancellation `shouldBe` "thread killed"
            Right model -> expectationFailure $ "Expected cancellation, got model " <> show model
        withResource pool $ \conn -> do
            [Only durableEventCount] <-
                query_ conn $
                    "select count(*) from " <> quoteIdent (getEventTableName eventTable)
            durableEventCount `shouldBe` (0 :: Int64)
        getModel p NoIndex `shouldReturn` 0

    it "propagates deferred commit failures without publishing uncommitted state" $ \(p, pool) -> do
        let tableName = getEventTableName eventTable
            constraintName = tableName <> "_event_unique"
        withResource pool $ \conn ->
            void $
                execute_ conn $
                    "alter table "
                        <> quoteIdent tableName
                        <> " add constraint "
                        <> quoteIdent constraintName
                        <> " unique (event) deferrable initially deferred"

        let failingLogger _ = fail "logger failure"
            backendWithFailingLogger = p{logger = failingLogger}
        result <-
            try @IO @SqlError $
                runCmd backendWithFailingLogger NoIndex $ \_ ->
                    pure (id, [AddOne, AddOne])
        case result of
            Left commitError -> sqlState commitError `shouldBe` "23505"
            Right model -> expectationFailure $ "Expected commit failure, got model " <> show model

        withResource pool $ \conn -> do
            [Only durableEventCount] <-
                query_ conn $
                    "select count(*) from " <> quoteIdent tableName
            durableEventCount `shouldBe` (0 :: Int64)

        getModel backendWithFailingLogger NoIndex `shouldReturn` 0
        freshBackend <- postgresWriteModel pool eventTable applyTestEvent 0
        getModel freshBackend NoIndex `shouldReturn` 0

slowId :: a -> a
slowId a = unsafePerformIO $ do
    threadDelay 250000
    pure a

loggingSpec :: SpecWith (PostgresEvent NoIndex TestModel TestEvent, Pool Connection)
loggingSpec = describe "Callstacks" $ do
    it "Callstack for runCmd reference this file" $ \(p', _) -> do
        (logVar, p) <- withStmLogger p'
        _ <- runCmd p NoIndex $ \_ -> pure (id, [AddOne])
        referencesThisFile =<< readTVarIO logVar
    it "Callstack for getModel reference this file" $ \(p', _) -> do
        (logVar, p) <- withStmLogger p'
        _ <- getModel p NoIndex
        referencesThisFile =<< readTVarIO logVar
    it "Callstack for getEventStream references this file" $ \(p', _) -> do
        (logVar, p) <- withStmLogger p'
        _ <- Stream.toList $ getEventStream p NoIndex
        referencesThisFile =<< readTVarIO logVar
    it "Callstack for getEventList references this file" $ \(p', _) -> do
        (logVar, p) <- withStmLogger p'
        _ <- getEventList p NoIndex
        referencesThisFile =<< readTVarIO logVar
  where
    referencesThisFile :: [LogEntry] -> IO ()
    referencesThisFile logs = do
        let thisFile = "DomainDriven/Persistance/PostgresSpec.hs"
        logs `shouldSatisfy` (not . null)
        logs `shouldSatisfy` all ((thisFile `L.isInfixOf`) . show)
    withStmLogger
        :: PostgresEvent NoIndex TestModel TestEvent
        -> IO (TVar [LogEntry], PostgresEvent NoIndex TestModel TestEvent)
    withStmLogger p = do
        logVar <- newTVarIO []
        pure (logVar, p{logger = \s -> atomically $ modifyTVar logVar (s :)})

type TestPersistance = PostgresEvent NoIndex TestModel TestEvent

noopMigration :: EventMigration
noopMigration _ _ _ = pure ()

-- | Copies the events unchanged.
copyMigration :: EventMigration
copyMigration prevName name conn = migrate1to1 @NoIndex @Value conn prevName name id

-- | Copies the events unchanged and counts how many times it ran.
probeMigration :: IORef Int -> EventMigration
probeMigration probe prevName name conn = do
    modifyIORef' probe (+ 1)
    copyMigration prevName name conn

-- | Signals when it starts (i.e. once the migrator holds its locks), then copies the
-- events slowly enough for a concurrent reader to race it.
slowMigration :: MVar () -> EventMigration
slowMigration started prevName name conn = do
    putMVar started ()
    migrate1to1 @NoIndex @Value conn prevName name slowId

startPersistance :: Pool Connection -> EventTable -> IO TestPersistance
startPersistance pool et = postgresWriteModel pool et applyTestEvent 0

addEvents :: TestPersistance -> Int -> IO TestModel
addEvents p n = runCmd p NoIndex $ \_ -> pure (id, replicate n AddOne)

-- | Drops any event tables left for the base name by an earlier run.
freshBase :: EventTableBaseName -> IO EventTableBaseName
freshBase base = base <$ withTestConn (`dropEventTables` base)

tablesInSchema :: Connection -> String -> IO [String]
tablesInSchema conn schema =
    fmap fromOnly
        <$> query
            conn
            "select tablename from pg_tables where schemaname = ? order by tablename"
            (Only schema)

-- | Starts a slow migration and, once it holds its locks, a second instance of the same
-- chain. The second one must wait for the first and then find nothing left to do.
secondStarterWaits :: Pool Connection -> IO ()
secondStarterWaits pool = do
    base <- freshBase "test_mig_second_starter"
    p <- startPersistance pool (TableName base 1)
    addEvents p 3 `shouldReturn` 3
    started <- newEmptyMVar
    probe <- newIORef (0 :: Int)
    (p1, p2) <-
        concurrently
            (startPersistance pool (MigrateTo 2 (slowMigration started) $ TableName base 1))
            ( do
                readMVar started
                startPersistance pool (MigrateTo 2 (probeMigration probe) $ TableName base 1)
            )
    readIORef probe `shouldReturn` 0
    getModel p1 NoIndex `shouldReturn` 3
    getModel p2 NoIndex `shouldReturn` 3
    withTestConn $ \conn -> do
        existingEventTableVersions conn base `shouldReturn` [1, 2]
        countRows conn (eventTableNameFor base 2) `shouldReturn` 3

versionedMigrationsSpec :: SpecWith (Pool Connection)
versionedMigrationsSpec = describe "versioned migrations" $ do
    describe "database without tables for the base name" $ do
        it "creates only the current table and runs no migration" $ \pool -> do
            base <- freshBase "test_mig_fresh"
            probe <- newIORef (0 :: Int)
            p <-
                startPersistance pool $
                    MigrateTo 5 (probeMigration probe)
                        . MigrateTo 4 (probeMigration probe)
                        $ TableName base 3
            addEvents p 2 `shouldReturn` 2
            readIORef probe `shouldReturn` 0
            withTestConn $ \conn -> existingEventTableVersions conn base `shouldReturn` [5]

        it "concurrent starts create one table" $ \pool -> do
            base <- freshBase "test_mig_concurrent_fresh"
            let et :: EventTable
                et = MigrateTo 2 noopMigration $ TableName base 1
            (p1, p2) <- concurrently (startPersistance pool et) (startPersistance pool et)
            addEvents p1 1 `shouldReturn` 1
            addEvents p2 1 `shouldReturn` 2
            withTestConn $ \conn -> existingEventTableVersions conn base `shouldReturn` [2]

    describe "existing database" $ do
        it "runs every pending step, each reading the table the previous one produced" $ \pool -> do
            base <- freshBase "test_mig_two_steps"
            p1 <- startPersistance pool (TableName base 1)
            addEvents p1 2 `shouldReturn` 2
            let negateEvents :: EventMigration
                negateEvents prevName name conn =
                    migrate1to1 @NoIndex @TestEvent conn prevName name $ fmap $ \case
                        AddOne -> SubtractOne
                        SubtractOne -> AddOne
                        Reset -> Reset
            p3 <-
                startPersistance pool $
                    MigrateTo 3 copyMigration . MigrateTo 2 negateEvents $
                        TableName base 1
            getModel p3 NoIndex `shouldReturn` (-2)
            withTestConn $ \conn -> do
                existingEventTableVersions conn base `shouldReturn` [1, 2, 3]
                countRows conn (eventTableNameFor base 3) `shouldReturn` 2
                isRetired conn (eventTableNameFor base 1) `shouldReturn` True
                isRetired conn (eventTableNameFor base 2) `shouldReturn` True
                isRetired conn (eventTableNameFor base 3) `shouldReturn` False

        it "makes a second starter wait for the migration and then run nothing" $ \pool ->
            secondStarterWaits pool

        it "makes a second starter wait even when the default isolation level is repeatable read" $ \_ ->
            withPoolRunning "set default_transaction_isolation = 'repeatable read'" secondStarterWaits

        it "restarting with the same chain runs nothing" $ \pool -> do
            base <- freshBase "test_mig_restart"
            probe <- newIORef (0 :: Int)
            let et :: EventTable
                et = MigrateTo 3 (probeMigration probe) . MigrateTo 2 noopMigration $ TableName base 1
            p <- startPersistance pool et
            addEvents p 3 `shouldReturn` 3
            p' <- startPersistance pool et
            getModel p' NoIndex `shouldReturn` 3
            readIORef probe `shouldReturn` 0
            withTestConn $ \conn -> existingEventTableVersions conn base `shouldReturn` [3]

        it "copies the events and retires the previous table; trimming the chain afterwards runs nothing" $ \pool -> do
            base <- freshBase "test_mig_forward"
            p1 <- startPersistance pool (TableName base 1)
            addEvents p1 2 `shouldReturn` 2
            probe <- newIORef (0 :: Int)
            p2 <- startPersistance pool (MigrateTo 2 (probeMigration probe) $ TableName base 1)
            readIORef probe `shouldReturn` 1
            getModel p2 NoIndex `shouldReturn` 2
            withTestConn $ \conn -> do
                countRows conn (eventTableNameFor base 2) `shouldReturn` 2
                isRetired conn (eventTableNameFor base 1) `shouldReturn` True
            addEvents p1 1 `shouldThrow` \e -> sqlErrorMsg e == "Event table has been retired."
            p3 <- startPersistance pool (TableName base 2)
            addEvents p3 1 `shouldReturn` 3
            readIORef probe `shouldReturn` 1
            withTestConn $ \conn -> existingEventTableVersions conn base `shouldReturn` [1, 2]

        it "continues from the highest table of a chain built by earlier versions" $ \pool -> do
            base <- freshBase "test_mig_handbuilt"
            withTestConn $ \conn -> do
                createEventTable' conn (eventTableNameFor base 1)
                retireTable conn (eventTableNameFor base 1)
                createEventTable' conn (eventTableNameFor base 2)
                evs <-
                    traverse
                        (\e -> Stored e (UTCTime (fromGregorian 2020 10 15) 0) <$> mkId)
                        [AddOne, AddOne, AddOne]
                void $ writeEvents conn (eventTableNameFor base 2) NoIndex evs
            probe <- newIORef (0 :: Int)
            p <- startPersistance pool (MigrateTo 2 (probeMigration probe) $ TableName base 1)
            getModel p NoIndex `shouldReturn` 3
            readIORef probe `shouldReturn` 0
            p' <-
                startPersistance pool $
                    MigrateTo 3 (probeMigration probe)
                        . MigrateTo 2 (probeMigration probe)
                        $ TableName base 1
            readIORef probe `shouldReturn` 1
            getModel p' NoIndex `shouldReturn` 3
            withTestConn $ \conn -> countRows conn (eventTableNameFor base 3) `shouldReturn` 3

        it "does not need the tables below the current one" $ \pool -> do
            base <- freshBase "test_mig_gap"
            p <- startPersistance pool (MigrateTo 2 noopMigration $ TableName base 1)
            addEvents p 2 `shouldReturn` 2
            probe <- newIORef (0 :: Int)
            p' <-
                startPersistance pool $
                    MigrateTo 3 (probeMigration probe)
                        . MigrateTo 2 noopMigration
                        $ TableName base 1
            readIORef probe `shouldReturn` 1
            getModel p' NoIndex `shouldReturn` 2
            withTestConn $ \conn -> existingEventTableVersions conn base `shouldReturn` [2, 3]

        it "keeps serving reads during a slow migration" $ \pool -> do
            base <- freshBase "test_mig_reads"
            p <- startPersistance pool (TableName base 1)
            addEvents p 3 `shouldReturn` 3
            getModel p NoIndex `shouldReturn` 3
            started <- newEmptyMVar
            (readDuration, _) <-
                concurrently
                    ( do
                        readMVar started
                        t0 <- getCurrentTime
                        evs <- getEventList p NoIndex
                        length evs `shouldBe` 3
                        getModel p NoIndex `shouldReturn` 3
                        t1 <- getCurrentTime
                        pure $ diffUTCTime t1 t0
                    )
                    (startPersistance pool (MigrateTo 2 (slowMigration started) $ TableName base 1))
            -- the copy takes >= 0.75s (3 events x 250ms); reads must not wait for it
            readDuration `shouldSatisfy` (< 0.5)

        it "rolls back every step when a later step fails" $ \pool -> do
            base <- freshBase "test_mig_rollback"
            p1 <- startPersistance pool (TableName base 1)
            addEvents p1 2 `shouldReturn` 2
            probe <- newIORef (0 :: Int)
            let broken :: EventTable
                broken =
                    MigrateTo 3 (\_ _ _ -> throwIO MigrationFailure)
                        . MigrateTo 2 (probeMigration probe)
                        $ TableName base 1
            startPersistance pool broken `shouldThrow` (== MigrationFailure)
            readIORef probe `shouldReturn` 1
            withTestConn $ \conn -> do
                existingEventTableVersions conn base `shouldReturn` [1]
                isRetired conn (eventTableNameFor base 1) `shouldReturn` False
            addEvents p1 1 `shouldReturn` 3

        it "finds tables through the search path instead of creating shadows" $ \pool -> do
            base <- freshBase "test_mig_search_path"
            p <- startPersistance pool (TableName base 1)
            addEvents p 2 `shouldReturn` 2
            let emptySchema :: String
                emptySchema = "test_mig_empty_schema"
            withSchema emptySchema $
                withPoolRunning ("set search_path = " <> quoteIdent emptySchema <> ", public") $ \shadowedPool -> do
                    p' <- startPersistance shadowedPool (TableName base 1)
                    getModel p' NoIndex `shouldReturn` 2
                    migrated <- startPersistance shadowedPool (MigrateTo 2 copyMigration $ TableName base 1)
                    getModel migrated NoIndex `shouldReturn` 2
                    restarted <- startPersistance shadowedPool (TableName base 2)
                    getModel restarted NoIndex `shouldReturn` 2
                    withTestConn $ \conn -> tablesInSchema conn emptySchema `shouldReturn` []
            withTestConn $ \conn -> isRetired conn (eventTableNameFor base 1) `shouldReturn` True
            restarted <- startPersistance pool (TableName base 2)
            getModel restarted NoIndex `shouldReturn` 2
            addEvents p 1 `shouldThrow` \e -> sqlErrorMsg e == "Event table has been retired."

        it "keeps tenant migrations separate from newer tables in a fallback schema" $ \pool -> do
            base <- freshBase "test_mig_tenant_versions"
            fallback <- startPersistance pool (TableName base 2)
            addEvents fallback 5 `shouldReturn` 5
            let schema :: String
                schema = "test_mig_tenant"
            withSchema schema $ do
                withPoolRunning ("set search_path = " <> quoteIdent schema) $ \tenantPool -> do
                    tenant <- startPersistance tenantPool (TableName base 1)
                    addEvents tenant 2 `shouldReturn` 2
                withPoolRunning ("set search_path = " <> quoteIdent schema <> ", public") $ \tenantPool -> do
                    tenant <- startPersistance tenantPool (TableName base 1)
                    getModel tenant NoIndex `shouldReturn` 2
                    migrated <- startPersistance tenantPool (MigrateTo 2 copyMigration $ TableName base 1)
                    getModel migrated NoIndex `shouldReturn` 2
                    addEvents migrated 1 `shouldReturn` 3
                    restarted <- startPersistance tenantPool (TableName base 2)
                    getModel restarted NoIndex `shouldReturn` 3
                    addEvents tenant 1 `shouldThrow` \e -> sqlErrorMsg e == "Event table has been retired."
                    withTestConn $ \conn ->
                        tablesInSchema conn schema
                            `shouldReturn` [eventTableNameFor base 1, eventTableNameFor base 2]
                fallbackRestarted <- startPersistance pool (TableName base 2)
                getModel fallbackRestarted NoIndex `shouldReturn` 5
                addEvents fallbackRestarted 1 `shouldReturn` 6

        it "ignores an unretired predecessor in a fallback schema" $ \pool -> do
            base <- freshBase "test_mig_tenant_predecessor"
            fallback <- startPersistance pool (TableName base 1)
            addEvents fallback 5 `shouldReturn` 5
            let schema :: String
                schema = "test_mig_tenant_current"
            withSchema schema $ do
                withPoolRunning ("set search_path = " <> quoteIdent schema) $ \tenantPool -> do
                    tenant <- startPersistance tenantPool (TableName base 2)
                    addEvents tenant 2 `shouldReturn` 2
                withPoolRunning ("set search_path = " <> quoteIdent schema <> ", public") $ \tenantPool -> do
                    tenant <- startPersistance tenantPool (TableName base 2)
                    getModel tenant NoIndex `shouldReturn` 2
                    migrated <- startPersistance tenantPool (MigrateTo 3 copyMigration $ TableName base 2)
                    getModel migrated NoIndex `shouldReturn` 2
                addEvents fallback 1 `shouldReturn` 6

        it "finds and migrates tables that live outside the public schema" $ \_ -> do
            let base :: EventTableBaseName
                base = "test_mig_own_schema_events"

                schema :: String
                schema = "test_mig_own_schema"
            withSchema schema $
                withPoolRunning ("set search_path = " <> quoteIdent schema) $ \schemaPool -> do
                    p <- startPersistance schemaPool (TableName base 1)
                    addEvents p 2 `shouldReturn` 2
                    probe <- newIORef (0 :: Int)
                    p' <- startPersistance schemaPool (MigrateTo 2 (probeMigration probe) $ TableName base 1)
                    readIORef probe `shouldReturn` 1
                    getModel p' NoIndex `shouldReturn` 2
                    restarted <- startPersistance schemaPool (TableName base 2)
                    getModel restarted NoIndex `shouldReturn` 2
                    withTestConn $ \conn -> do
                        tablesInSchema conn schema
                            `shouldReturn` [eventTableNameFor base 1, eventTableNameFor base 2]
                        existingEventTableVersions conn base `shouldReturn` []

        it "rejects retries after a committing migration until the incomplete table is removed" $ \pool -> do
            base <- freshBase "test_mig_Commits"
            p <- startPersistance pool (TableName base 1)
            addEvents p 2 `shouldReturn` 2
            let committing :: EventMigration
                committing prevName name conn =
                    withTransaction conn $ copyMigration prevName name conn

                failedChain :: EventTable
                failedChain = MigrateTo 2 committing $ TableName base 1
            startPersistance pool failedChain
                `shouldThrow` (== MigrationEndedTransaction (eventTableNameFor base 2))
            withTestConn $ \conn -> do
                existingEventTableVersions conn base `shouldReturn` [1, 2]
                isRetired conn (eventTableNameFor base 1) `shouldReturn` False
            addEvents p 1 `shouldReturn` 3
            probe <- newIORef (0 :: Int)
            for_ [failedChain, TableName base 2, MigrateTo 3 (probeMigration probe) failedChain] $ \chain ->
                startPersistance pool chain
                    `shouldThrow` (== IncompleteMigration (eventTableNameFor base 1) (eventTableNameFor base 2))
            readIORef probe `shouldReturn` 0
            withTestConn $ \conn ->
                void . execute_ conn $ "drop table " <> quoteIdent (eventTableNameFor base 2)
            repaired <- startPersistance pool (MigrateTo 2 copyMigration $ TableName base 1)
            getModel repaired NoIndex `shouldReturn` 3
            restarted <- startPersistance pool (TableName base 2)
            getModel restarted NoIndex `shouldReturn` 3
            addEvents p 1 `shouldThrow` \e -> sqlErrorMsg e == "Event table has been retired."

        it "rejects a waiting starter when a migration commits before retiring the previous table" $ \pool -> do
            base <- freshBase "test_mig_commits_waiting"
            p <- startPersistance pool (TableName base 1)
            addEvents p 2 `shouldReturn` 2
            migrationStarted <- newEmptyMVar
            starterWaiting <- newEmptyMVar
            let committing :: EventMigration
                committing prevName name conn = do
                    copyMigration prevName name conn
                    putMVar migrationStarted ()
                    takeMVar starterWaiting
                    commit conn

                waitingLogger :: LogEntry -> IO ()
                waitingLogger = \case
                    WaitingForMigrationLock{} -> putMVar starterWaiting ()
                    DbTransactionDuration{} -> pure ()
                    EventTableLockDuration{} -> pure ()
                    EventTableMigrationDuration{} -> pure ()
                    WaitForConnectionDuration{} -> pure ()

                chain :: EventTable
                chain = MigrateTo 2 committing $ TableName base 1
            outcome <- timeout 5000000 $
                concurrently
                    (try @IO @MigrationError . void $ startPersistance pool chain)
                    ( do
                        readMVar migrationStarted
                        try @IO @MigrationError . void $
                            postgresWriteModelWith
                                (\backend -> backend{logger = waitingLogger})
                                pool
                                chain
                                applyTestEvent
                                0
                    )
            outcome
                `shouldBe` Just
                    ( Left $ MigrationEndedTransaction (eventTableNameFor base 2)
                    , Left $ IncompleteMigration (eventTableNameFor base 1) (eventTableNameFor base 2)
                    )
            addEvents p 1 `shouldReturn` 3

        it "checks only the previous existing table, even across gaps" $ \pool -> do
            base <- freshBase "test_mig_previous_existing"
            p <- startPersistance pool (TableName base 1)
            withTestConn $ \conn -> do
                createEventTable' conn (eventTableNameFor base 3)
                retireTable conn (eventTableNameFor base 3)
                createEventTable' conn (eventTableNameFor base 5)
            void $ startPersistance pool (TableName base 5)
            addEvents p 1 `shouldReturn` 1
            withTestConn $ \conn ->
                void . execute_ conn $ "drop table " <> quoteIdent (eventTableNameFor base 3)
            startPersistance pool (TableName base 5)
                `shouldThrow` (== IncompleteMigration (eventTableNameFor base 1) (eventTableNameFor base 5))

        it "requires the previous table's retirement trigger to be enabled for ordinary writers" $ \pool -> do
            base <- freshBase "test_mig_retirement_enabled"
            _ <- startPersistance pool (TableName base 1)
            let chain :: EventTable
                chain = MigrateTo 2 copyMigration $ TableName base 1
            _ <- startPersistance pool chain
            for_ [("disable", False), ("enable replica", False), ("enable always", True), ("enable", True)] $ \(mode, retired) -> do
                withTestConn $ \conn ->
                    void . execute_ conn $
                        "alter table " <> quoteIdent (eventTableNameFor base 1) <> " " <> mode <> " trigger retired"
                if retired
                    then void $ startPersistance pool chain
                    else
                        startPersistance pool chain
                            `shouldThrow` (== IncompleteMigration (eventTableNameFor base 1) (eventTableNameFor base 2))

        it "fails when a migration rolls back the startup transaction, leaving the previous table live" $ \pool -> do
            base <- freshBase "test_mig_rolls_back"
            p <- startPersistance pool (TableName base 1)
            addEvents p 2 `shouldReturn` 2
            startPersistance pool (MigrateTo 2 (\_ _ conn -> rollback conn) $ TableName base 1)
                `shouldThrow` (== MigrationEndedTransaction (eventTableNameFor base 2))
            withTestConn $ \conn -> do
                existingEventTableVersions conn base `shouldReturn` [1]
                isRetired conn (eventTableNameFor base 1) `shouldReturn` False
            addEvents p 1 `shouldReturn` 3
            repaired <- startPersistance pool (MigrateTo 2 copyMigration $ TableName base 1)
            getModel repaired NoIndex `shouldReturn` 3

    describe "refusing to start" $ do
        it "when the database is ahead of the code" $ \pool -> do
            base <- freshBase "test_mig_ahead"
            _ <- startPersistance pool (MigrateTo 2 noopMigration $ TableName base 1)
            startPersistance pool (TableName base 1)
                `shouldThrow` (== DatabaseAheadOfCode base 2 1)

        it "when the database is behind the code's TableName version" $ \pool -> do
            base <- freshBase "test_mig_behind"
            p <- startPersistance pool (TableName base 1)
            addEvents p 2 `shouldReturn` 2
            startPersistance pool (MigrateTo 4 noopMigration $ TableName base 3)
                `shouldThrow` (== DatabaseBelowBaseVersion base 1 3)
            withTestConn $ \conn -> existingEventTableVersions conn base `shouldReturn` [1]

        it "when the chain is invalid, before connecting" $ \_ -> do
            pool <- simplePool (throwIO (userError "connected") :: IO Connection)
            let base :: EventTableBaseName
                base = "test_mig_invalid"
            startPersistance pool (MigrateTo 3 noopMigration $ TableName base 1)
                `shouldThrow` (== MisnumberedMigration base 2 3)
            startPersistance pool (TableName base 0)
                `shouldThrow` (== InvalidEventTableVersion base 0)
            startPersistance pool (TableName (replicate 61 'a') 1)
                `shouldThrow` (== EventTableNameTooLong (replicate 61 'a' <> "_v1"))

        it "when runMigrations is handed an invalid chain directly" $ \pool -> do
            base <- freshBase "test_mig_direct"
            p <- startPersistance pool (TableName base 1)
            withIOTrans
                p
                ( \pgt ->
                    runMigrations
                        (const $ pure ())
                        (transaction pgt)
                        (MigrateTo 3 noopMigration $ TableName base 1)
                )
                `shouldThrow` (== MisnumberedMigration base 2 3)
            withTestConn $ \conn -> existingEventTableVersions conn base `shouldReturn` [1]

    it "keeps prefix bases apart (foo vs foo_v2)" $ \pool -> do
        base <- freshBase "test_mig_prefix"
        prefixBase <- freshBase (base <> "_v2")
        pPrefix <- startPersistance pool (TableName prefixBase 1)
        addEvents pPrefix 1 `shouldReturn` 1
        p <- startPersistance pool (MigrateTo 2 noopMigration $ TableName base 1)
        addEvents p 2 `shouldReturn` 2
        withTestConn $ \conn -> do
            existingEventTableVersions conn base `shouldReturn` [2]
            existingEventTableVersions conn prefixBase `shouldReturn` [1]
            dropEventTables conn base
            existingEventTableVersions conn prefixBase `shouldReturn` [1]

    it "reports waiting for the migration lock to a custom logger, only when it waits" $ \pool -> do
        base <- freshBase "test_mig_lock_log"
        logVar <- newTVarIO []
        let waitedFor :: [LogEntry] -> [EventTableBaseName]
            waitedFor entries = [waited | WaitingForMigrationLock waited <- entries]

            start :: IO TestPersistance
            start =
                postgresWriteModelWith
                    (\p -> p{logger = \entry -> atomically $ modifyTVar logVar (entry :)})
                    pool
                    (TableName base 1)
                    applyTestEvent
                    0
        _ <- start
        waitedFor <$> readTVarIO logVar `shouldReturn` []
        withTestConn $ \conn -> do
            begin conn
            migrationLock (const $ pure ()) conn base
            withAsync start $ \blocked -> do
                logged <-
                    timeout 5000000 . atomically $
                        readTVar logVar >>= checkSTM . not . null . waitedFor
                commit conn
                void $ wait blocked
                logged `shouldBe` Just ()
        waitedFor <$> readTVarIO logVar `shouldReturn` [base]
