{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Missing NOINLINE pragma" #-}
module DomainDriven.Persistance.PostgresSpec where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.DeepSeq (NFData (rnf))
import Control.Exception
    ( AsyncException (ThreadKilled)
    , ErrorCall
    , SomeAsyncException
    , SomeException
    , bracket
    , bracket_
    , displayException
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
import Data.IORef (newIORef, readIORef)
import Data.Int (Int64)
import Data.List qualified as L
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (fromString)
import Data.Text qualified as T
import Data.Time
import Data.Traversable
import Data.UUID (UUID, nil)
import Data.UUID.V4 qualified as V4
import Database.PostgreSQL.Simple
import DomainDriven.Persistance.Class
import DomainDriven.Persistance.Postgres
import DomainDriven.Persistance.Postgres.Internal
    ( LogEntry (..)
    , getCurrentState
    , getEventTableName
    , parseEventRows
    , publishNumberedModel
    , queryHasEventsAfter
    , queryEvents
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
    , concurrently
    , forConcurrently
    , modifyTVar
    , newTVarIO
    , readTVarIO
    , try
    , wait
    , withAsync
    )
import UnliftIO.Pool
import Prelude

eventTable :: EventTable
eventTable =
    MigrateUsing (\_ _ _ -> pure ())
        . MigrateUsing (\_ _ _ -> pure ())
        $ InitialVersion
            "test_events"

eventTable2 :: EventTable
eventTable2 = MigrateUsing mig eventTable
  where
    mig :: PreviousEventTableName -> EventTableName -> Connection -> IO ()
    mig prevName name conn = migrate1to1 @NoIndex @Value conn prevName name id

lockEventTable1 :: EventTable
lockEventTable1 = InitialVersion "test_lock_events_1"

lockEventTable2 :: EventTable
lockEventTable2 = InitialVersion "test_lock_events_2"

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

type TestModel = Int

data TestEvent
    = AddOne
    | SubtractOne
    | Reset
    deriving (Show, Eq, Generic, FromJSON, ToJSON, NFData)

data ForceProbeEvent = ForceProbeEvent ~String

instance FromJSON ForceProbeEvent where
    parseJSON = withObject "ForceProbeEvent" $ \o -> do
        shouldThrowOnForce <- o .: "shouldThrowOnForce"
        let payload =
                if shouldThrowOnForce
                    then error "force-probe"
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
    dropEventTables =<< mkTestConn
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
    dropEventTables =<< mkTestConn
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
    cleanupTables = bracket mkTestConn close $ \conn ->
        traverse_
            (dropEventTableChain conn)
            [lockEventTable1, lockEventTable2]

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

dropEventTables :: Connection -> IO ()
dropEventTables conn = do
    testTables <-
        query_
            conn
            "select table_name from information_schema.tables where table_name like 'test_events_v%'"
            :: IO [Only String]
    traverse_
        (\t -> execute_ conn ("drop table \"" <> fromString (fromOnly t) <> "\""))
        testTables

dropEventTableChain :: Connection -> EventTable -> IO ()
dropEventTableChain conn et =
    traverse_ dropTable (reverse $ tableNames et)
  where
    dropTable tableName =
        void $ execute_ conn ("drop table if exists " <> quoteIdent tableName)

tableNames :: EventTable -> [EventTableName]
tableNames et = case et of
    MigrateUsing _ next -> getEventTableName et : tableNames next
    InitialVersion{} -> [getEventTableName et]

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

        parseEventRows @ForceProbeEvent 1 testChunkSize [row] `shouldThrow` \e ->
            "force-probe" `L.isInfixOf` displayException (e :: SomeException)

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

    it "rejects invalid raw table names before acquiring a connection" $ \(_p, _pool) -> do
        let invalidNames :: [EventTableName]
            invalidNames = ["", "bad;name", "bad\"name", "bad\0name", replicate 32 'é']
                <> fmap (replicate 63 'a' <>) ["x", "y"]
        bracket (simplePool (fail "Unexpected connection acquisition")) destroyAllResources $ \pool ->
            for_ invalidNames $ \tableName ->
                ( postgresWriteModelNoMigration pool tableName applyTestEvent 0
                    :: IO (PostgresEvent Indexed TestModel TestEvent)
                ) `shouldThrow` \(_ :: ErrorCall) -> True

    it "accepts a 63-character raw table name without aliasing its suffixes" $ \(_p, pool) -> do
        let tableName :: EventTableName
            tableName = "test_events_v" <> replicate 50 'a'
        backend <- postgresWriteModelNoMigration pool tableName applyTestEvent 0
        runCmd backend (Indexed "a") (\_ -> pure (id, [AddOne])) `shouldReturn` 1
        for_ ["x", "y"] $ \suffix -> do
            ( postgresWriteModelNoMigration pool (tableName <> suffix) applyTestEvent 0
                :: IO (PostgresEvent Indexed TestModel TestEvent)
                ) `shouldThrow` \(_ :: ErrorCall) -> True
            let alias :: PostgresEvent Indexed TestModel TestEvent
                alias = backend{eventTableName = tableName <> suffix}
            runCmd alias (Indexed "a") (\_ -> pure (id, [AddOne]))
                `shouldThrow` \(_ :: ErrorCall) -> True
            getModel alias (Indexed "a") `shouldThrow` \(_ :: ErrorCall) -> True
            getEventList alias (Indexed "a") `shouldThrow` \(_ :: ErrorCall) -> True
            Stream.toList (getEventStream alias (Indexed "a"))
                `shouldThrow` \(_ :: ErrorCall) -> True
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
                MigrateUsing
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
            migrated = MigrateUsing (\prev next conn -> migrate1to1 @Indexed @Value conn prev next id) eventTable
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
                MigrateUsing (\prev next conn -> migrate1to1 @Indexed @Value conn prev next id) eventTable

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

    for_ [("0", 10000000), ("100ms", 2000000)] $ \(configuredTimeout, deadline) ->
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
                migrated = MigrateUsing (\prev next conn -> migrate1to1 @Indexed @Value conn prev next id) eventTable

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
                                    wait migrator
                            )
                    case outcome of
                        Nothing -> expectationFailure "Nested command and migration did not finish before the deadline"
                        Just (writer, migratedBackend) -> do
                            writer `shouldSatisfy` \case
                                Left err -> sqlState err == "55P03"
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
            eventTableBroken = MigrateUsing (\_ _ _ -> error "ops") eventTable2

        postgresWriteModel pool eventTableBroken applyTestEvent 0
            `shouldThrow` const @_ @SomeException True
        conn <- mkTestConn

        case tableNames eventTableBroken of
            failedMig : prevMig : _ -> do
                [Only prevExists] <-
                    query_ @(Only Bool) conn $
                        "select exists(select * from pg_tables where tablename='"
                            <> fromString prevMig
                            <> "')"
                [Only brokenExists] <-
                    query_ @(Only Bool) conn $
                        "select exists(select * from pg_tables where tablename='"
                            <> fromString failedMig
                            <> "')"
                prevExists `shouldBe` True
                brokenExists `shouldBe` False
            _ -> fail "Unexpectedly lacking table versions!"

    it "migrate1toManyWithState threads state in order and resets it per index" $ \(_p, pool) -> do
        let statefulTable :: EventTable
            statefulTable = InitialVersion "test_events_stateful"

            migratedTable :: EventTable
            migratedTable = MigrateUsing statefulMigration statefulTable

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

        withResource pool (`dropEventTableChain` migratedTable)
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
                    (MigrateUsing mig eventTable2)
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
