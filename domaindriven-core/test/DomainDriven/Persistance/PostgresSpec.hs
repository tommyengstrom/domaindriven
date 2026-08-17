{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Missing NOINLINE pragma" #-}
module DomainDriven.Persistance.PostgresSpec where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.DeepSeq (NFData (rnf))
import Control.Exception (ErrorCall, SomeException, bracket, bracket_, displayException)
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
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
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
    ( LogEntry (..)
    , advisoryXactLock
    , countRows
    , createEventTable'
    , eventTableNameFor
    , existingEventTableVersions
    , getEventTableName
    , insertMigrationRow
    , isRetired
    , migrationsTableName
    , parseEventRows
    , queryEvents
    , retireTable
    , tableExists
    , writeEvents
    , writerLockKey
    )
import DomainDriven.Persistance.Postgres.Migration
import DomainDriven.Persistance.Postgres.Types
    ( EventNumber (..)
    , EventRowOut (..)
    , PersistanceError (..)
    , quoteIdent
    )
import GHC.Generics (Generic)
import GHC.IO.Unsafe (unsafePerformIO)
import Streamly.Data.Stream.Prelude qualified as Stream
import Test.Hspec
import UnliftIO
    ( MVar
    , TVar
    , async
    , atomically
    , concurrently
    , forConcurrently
    , modifyTVar
    , newEmptyMVar
    , newTVarIO
    , putMVar
    , readMVar
    , readTVarIO
    , try
    , wait
    )
import UnliftIO.Pool
import Prelude

testEventsBase :: EventTableBaseName
testEventsBase = "test_events"

-- | Two no-op migrations on top of the base version. On a fresh database only the
-- current table (test_events_v3) is created and the migrations never run.
eventTable :: EventTable
eventTable =
    MigrateWith "noop-2" (\_ _ _ -> pure ())
        . MigrateWith "noop-1" (\_ _ _ -> pure ())
        $ TableName testEventsBase 1

eventTable2 :: EventTable
eventTable2 = MigrateWith "copy-events" mig eventTable
  where
    mig :: PreviousEventTableName -> EventTableName -> Connection -> IO ()
    mig prevName name conn = migrate1to1 @NoIndex @Value conn prevName name id

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
    around (setupPersistance noHook) loggingSpec
    around setupPersistanceIndexed indexedSpec
    around setupTableScopedLocks tableScopedLockSpec
    around withTestPool migrationTagsSpec

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
    let stripesAndResources = 5
    poolCfg <-
        setNumStripes (Just stripesAndResources)
            <$> mkDefaultPoolConfig mkTestConn close 1 stripesAndResources
    pool <- newPool poolCfg
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
mkTestConn =
    connect $
        ConnectInfo
            { connectHost = "localhost"
            , connectPort = 5432
            , connectUser = "postgres"
            , connectPassword = "postgres"
            , connectDatabase = "domaindriven"
            }

withTestConn :: (Connection -> IO a) -> IO a
withTestConn = bracket mkTestConn close

withTestPool :: (Pool Connection -> IO ()) -> IO ()
withTestPool = bracket (simplePool mkTestConn) destroyAllResources

-- | The metadata rows for a base name: (version, tag, origin).
migrationRows :: Connection -> EventTableBaseName -> IO [(Int, Maybe String, String)]
migrationRows conn base =
    query
        conn
        ( "select version, tag, origin from "
            <> quoteIdent migrationsTableName
            <> " where base_name = ? order by version"
        )
        (Only base)

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
            eventTableBroken = MigrateWith "broken" (\_ _ _ -> error "ops") eventTable2

        postgresWriteModel pool eventTableBroken applyTestEvent 0
            `shouldThrow` const @_ @SomeException True

        withTestConn $ \conn -> do
            tableExists conn (getEventTableName eventTable2) `shouldReturn` True
            tableExists conn (getEventTableName eventTableBroken) `shouldReturn` False

    it "migrate1toManyWithState threads state in order and resets it per index" $ \(_p, pool) -> do
        let statefulTable :: EventTable
            statefulTable = TableName "test_events_stateful" 1

            migratedTable :: EventTable
            migratedTable = MigrateWith "stateful" statefulMigration statefulTable

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
                    (Indexed "a")
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
                    <$> queryEvents @Value conn (getEventTableName migratedTable) (Indexed "a")
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
                    (MigrateWith "slow-copy" mig eventTable2)
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

    slowId :: a -> a
    slowId a = unsafePerformIO $ do
        -- putStrLn "Migrating slowly..."
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
        logs `shouldSatisfy` all ((thisFile `L.isInfixOf`) . show)
    withStmLogger
        :: PostgresEvent NoIndex TestModel TestEvent
        -> IO (TVar [LogEntry], PostgresEvent NoIndex TestModel TestEvent)
    withStmLogger p = do
        logVar <- newTVarIO []
        pure (logVar, p{logger = \s -> atomically $ modifyTVar logVar (s :)})

--------------------------------------------------------------------------------
-- Tagged migrations: bootstrap, verification, adoption, concurrency
--------------------------------------------------------------------------------

type TestPersistance = PostgresEvent NoIndex TestModel TestEvent

noopMigration :: EventMigration
noopMigration _ _ _ = pure ()

-- | Copies the events unchanged and counts how many times it ran.
probeMigration :: IORef Int -> EventMigration
probeMigration probe prevName name conn = do
    modifyIORef' probe (+ 1)
    migrate1to1 @NoIndex @Value conn prevName name id

-- | Signals when it starts (i.e. once the migrator holds its locks), then copies the
-- events slowly enough for a concurrent writer/reader to race it.
slowMigration :: forall index. IsPgIndex index => MVar () -> EventMigration
slowMigration started prevName name conn = do
    putMVar started ()
    migrate1to1 @index @Value conn prevName name slowId
  where
    slowId :: a -> a
    slowId a = unsafePerformIO $ do
        threadDelay 250000
        pure a

startPersistance :: Pool Connection -> EventTable -> IO TestPersistance
startPersistance pool et = postgresWriteModel pool et applyTestEvent 0

addEvents :: TestPersistance -> Int -> IO TestModel
addEvents p n = runCmd p NoIndex $ \_ -> pure (id, replicate n AddOne)

migrationTagsSpec :: SpecWith (Pool Connection)
migrationTagsSpec = describe "tagged migrations" $ do
    describe "fresh database" $ do
        it "creates only the current table, records the whole chain and runs no migration" $ \pool -> do
            let base = "test_mig_fresh"
            withTestConn (`dropEventTables` base)
            probe <- newIORef (0 :: Int)
            let et =
                    MigrateWith "b" (probeMigration probe)
                        . MigrateWith "a" (probeMigration probe)
                        $ TableName base 3
            _ <- startPersistance pool et
            readIORef probe `shouldReturn` 0
            withTestConn $ \conn -> do
                existingEventTableVersions conn base `shouldReturn` [5]
                migrationRows conn base
                    `shouldReturn` [ (3, Nothing, "bootstrap")
                                   , (4, Just "a", "bootstrap")
                                   , (5, Just "b", "bootstrap")
                                   ]

        it "bootstraps directly at the current version when the chain starts above 1" $ \pool -> do
            let base = "test_mig_fresh_high"
            withTestConn (`dropEventTables` base)
            p <- startPersistance pool (TableName base 48)
            addEvents p 2 `shouldReturn` 2
            withTestConn $ \conn -> do
                existingEventTableVersions conn base `shouldReturn` [48]
                migrationRows conn base `shouldReturn` [(48, Nothing, "bootstrap")]

        it "two simultaneous bootstraps produce one table and one set of rows" $ \pool -> do
            let base = "test_mig_concurrent_bootstrap"
            withTestConn (`dropEventTables` base)
            probe <- newIORef (0 :: Int)
            let et = MigrateWith "a" (probeMigration probe) $ TableName base 1
            (p1, p2) <- concurrently (startPersistance pool et) (startPersistance pool et)
            addEvents p1 1 `shouldReturn` 1
            addEvents p2 1 `shouldReturn` 2
            readIORef probe `shouldReturn` 0
            withTestConn $ \conn -> do
                existingEventTableVersions conn base `shouldReturn` [2]
                migrationRows conn base
                    `shouldReturn` [(1, Nothing, "bootstrap"), (2, Just "a", "bootstrap")]

    describe "verification against the recorded history" $ do
        it "restarting with the same chain is a no-op" $ \pool -> do
            let base = "test_mig_restart"
            withTestConn (`dropEventTables` base)
            probe <- newIORef (0 :: Int)
            let et =
                    MigrateWith "b" (probeMigration probe) . MigrateWith "a" noopMigration $ TableName base 1
            p <- startPersistance pool et
            addEvents p 3 `shouldReturn` 3
            rowsBefore <- withTestConn (`migrationRows` base)
            p' <- startPersistance pool et
            getModel p' NoIndex `shouldReturn` 3
            readIORef probe `shouldReturn` 0
            withTestConn $ \conn -> do
                migrationRows conn base `shouldReturn` rowsBefore
                existingEventTableVersions conn base `shouldReturn` [3]

        it "a changed tag is a tag mismatch and runs no migration" $ \pool -> do
            let base = "test_mig_changed_tag"
            withTestConn (`dropEventTables` base)
            probe <- newIORef (0 :: Int)
            _ <-
                startPersistance
                    pool
                    (MigrateWith "b" noopMigration . MigrateWith "a" noopMigration $ TableName base 1)
            let changed =
                    MigrateWith "c" (probeMigration probe) . MigrateWith "a" noopMigration $ TableName base 1
            startPersistance pool changed `shouldThrow` \case
                MigrationTagMismatch b 1 [d] Nothing ->
                    b == base
                        && d
                            == TagDisagreement
                                { version = 3
                                , codeTag = "c"
                                , recordedTag = Just "b"
                                , recordedOrigin = Just OriginBootstrap
                                , codeTagRecordedAt = Nothing
                                }
                _ -> False
            r <- try @IO @MigrationError (startPersistance pool changed)
            case r of
                Left e -> do
                    displayException e
                        `shouldSatisfy` L.isInfixOf "test_mig_changed_tag_v3: code says \"c\", database recorded \"b\""
                    displayException e `shouldSatisfy` L.isInfixOf "recorded at bootstrap"
                Right _ -> expectationFailure "expected a MigrationError"
            readIORef probe `shouldReturn` 0

        it "a base version one too high is diagnosed as a shift, before any migration runs" $ \pool -> do
            let base = "test_mig_shift_high"
            withTestConn (`dropEventTables` base)
            probe <- newIORef (0 :: Int)
            _ <-
                startPersistance
                    pool
                    (MigrateWith "b" noopMigration . MigrateWith "a" noopMigration $ TableName base 1)
            let shifted =
                    MigrateWith "b" (probeMigration probe) . MigrateWith "a" (probeMigration probe) $
                        TableName base 2
            r <- try @IO @MigrationError (startPersistance pool shifted)
            case r of
                Left e@(MigrationTagMismatch _ 2 disagreements (Just (-1))) -> do
                    map (\d -> (version d, codeTag d, recordedTag d, codeTagRecordedAt d)) disagreements
                        `shouldBe` [(3, "a", Just "b", Just 2), (4, "b", Nothing, Just 3)]
                    displayException e
                        `shouldSatisfy` L.isInfixOf "test_mig_shift_high_v3: code says \"a\", database recorded \"b\""
                    displayException e
                        `shouldSatisfy` L.isInfixOf "matches the recorded history 1 version(s) earlier"
                    displayException e `shouldSatisfy` L.isInfixOf "base version is probably 1, not 2"
                other -> expectationFailure $ "unexpected result: " <> show (void other)
            readIORef probe `shouldReturn` 0
            withTestConn $ \conn -> existingEventTableVersions conn base `shouldReturn` [3]

        it "a base version one too low is diagnosed as a shift, before any migration runs" $ \pool -> do
            let base = "test_mig_shift_low"
            withTestConn (`dropEventTables` base)
            probe <- newIORef (0 :: Int)
            _ <-
                startPersistance
                    pool
                    (MigrateWith "b" noopMigration . MigrateWith "a" noopMigration $ TableName base 2)
            let shifted =
                    MigrateWith "b" (probeMigration probe) . MigrateWith "a" (probeMigration probe) $
                        TableName base 1
            r <- try @IO @MigrationError (startPersistance pool shifted)
            case r of
                Left e@(MigrationTagMismatch _ 1 _ (Just 1)) -> do
                    displayException e
                        `shouldSatisfy` L.isInfixOf "matches the recorded history 1 version(s) later"
                    displayException e `shouldSatisfy` L.isInfixOf "base version is probably 2, not 1"
                other -> expectationFailure $ "unexpected result: " <> show (void other)
            readIORef probe `shouldReturn` 0
            withTestConn $ \conn -> existingEventTableVersions conn base `shouldReturn` [4]

        it "a migration inserted mid-chain on a bootstrapped database is rejected before it runs" $ \pool -> do
            let base = "test_mig_mid_chain"
            withTestConn (`dropEventTables` base)
            probe <- newIORef (0 :: Int)
            _ <-
                startPersistance
                    pool
                    (MigrateWith "b" noopMigration . MigrateWith "a" noopMigration $ TableName base 1)
            let inserted =
                    MigrateWith "b" (probeMigration probe)
                        . MigrateWith "new" (probeMigration probe)
                        . MigrateWith "a" noopMigration
                        $ TableName base 1
            r <- try @IO @MigrationError (startPersistance pool inserted)
            case r of
                Left e@(MigrationTagMismatch _ 1 disagreements Nothing) -> do
                    map (\d -> (version d, codeTag d, recordedTag d, codeTagRecordedAt d)) disagreements
                        `shouldBe` [(3, "new", Just "b", Nothing), (4, "b", Nothing, Just 3)]
                    displayException e
                        `shouldSatisfy` L.isInfixOf "does not match the recorded history at any offset"
                other -> expectationFailure $ "unexpected result: " <> show (void other)
            readIORef probe `shouldReturn` 0
            withTestConn $ \conn -> existingEventTableVersions conn base `shouldReturn` [3]

        it "fails when the database is ahead of the code" $ \pool -> do
            let base = "test_mig_ahead"
            withTestConn (`dropEventTables` base)
            _ <- startPersistance pool (MigrateWith "a" noopMigration $ TableName base 1)
            startPersistance pool (TableName base 1)
                `shouldThrow` (== DatabaseAheadOfCode base 2 1)

        it "fails when the database is behind the code's base version, even for an empty table" $ \pool -> do
            let base = "test_mig_behind"
            withTestConn (`dropEventTables` base)
            p <- startPersistance pool (TableName base 1)
            addEvents p 2 `shouldReturn` 2
            startPersistance pool (TableName base 3)
                `shouldThrow` (== DatabaseBehindCodeBase base 1 3 2)
            withTestConn $ \conn -> existingEventTableVersions conn base `shouldReturn` [1]
            -- An empty stranded table is still a hard failure ...
            withTestConn (`dropEventTables` base)
            _ <- startPersistance pool (TableName base 1)
            startPersistance pool (TableName base 3)
                `shouldThrow` (== DatabaseBehindCodeBase base 1 3 0)
            -- ... whereas a database without any tables for the base bootstraps at v3.
            withTestConn (`dropEventTables` base)
            _ <- startPersistance pool (TableName base 3)
            withTestConn $ \conn -> do
                existingEventTableVersions conn base `shouldReturn` [3]
                migrationRows conn base `shouldReturn` [(3, Nothing, "bootstrap")]

        it "fails with a remediation when the tables are gone but the metadata remains" $ \pool -> do
            let base = "test_mig_table_missing"
            withTestConn (`dropEventTables` base)
            _ <- startPersistance pool (TableName base 1)
            withTestConn $ \conn ->
                void $ execute_ conn ("drop table " <> quoteIdent (eventTableNameFor base 1))
            r <- try @IO @MigrationError (startPersistance pool (TableName base 1))
            case r of
                Left e@(CurrentEventTableMissing b 1 _) -> do
                    b `shouldBe` base
                    displayException e
                        `shouldSatisfy` L.isInfixOf ("delete from domaindriven_migrations where base_name = '" <> base <> "'")
                other -> expectationFailure $ "unexpected result: " <> show (void other)
            -- dropEventTables is the reset that also clears the metadata
            withTestConn (`dropEventTables` base)
            p <- startPersistance pool (TableName base 1)
            addEvents p 1 `shouldReturn` 1

    describe "migrating forward" $ do
        it
            "copies the events, retires the previous table and records the step; deleting the migration afterwards is a no-op" $ \pool -> do
            let base = "test_mig_forward"
            withTestConn (`dropEventTables` base)
            p1 <- startPersistance pool (TableName base 1)
            addEvents p1 2 `shouldReturn` 2
            probe <- newIORef (0 :: Int)
            p2 <- startPersistance pool (MigrateWith "copy" (probeMigration probe) $ TableName base 1)
            readIORef probe `shouldReturn` 1
            getModel p2 NoIndex `shouldReturn` 2
            withTestConn $ \conn -> do
                countRows conn (eventTableNameFor base 2) `shouldReturn` 2
                isRetired conn (eventTableNameFor base 1) `shouldReturn` True
                migrationRows conn base
                    `shouldReturn` [(1, Nothing, "bootstrap"), (2, Just "copy", "migration")]
            -- The old writer now fails on the retired table
            addEvents p1 1 `shouldThrow` \e -> sqlErrorMsg e == "Event table has been retired."
            -- Delete the migration from code and bump the base version: verify-only, no-op
            p3 <- startPersistance pool (TableName base 2)
            getModel p3 NoIndex `shouldReturn` 2
            addEvents p3 1 `shouldReturn` 3
            readIORef probe `shouldReturn` 1
            withTestConn $ \conn -> do
                migrationRows conn base
                    `shouldReturn` [(1, Nothing, "bootstrap"), (2, Just "copy", "migration")]
                existingEventTableVersions conn base `shouldReturn` [1, 2]

        it "two simultaneous migrators copy exactly once" $ \pool -> do
            let base = "test_mig_concurrent_migrate"
            withTestConn (`dropEventTables` base)
            p1 <- startPersistance pool (TableName base 1)
            addEvents p1 3 `shouldReturn` 3
            probe <- newIORef (0 :: Int)
            let et = MigrateWith "copy" (probeMigration probe) $ TableName base 1
            (p2, p3) <- concurrently (startPersistance pool et) (startPersistance pool et)
            readIORef probe `shouldReturn` 1
            getModel p2 NoIndex `shouldReturn` 3
            getModel p3 NoIndex `shouldReturn` 3
            withTestConn $ \conn -> do
                countRows conn (eventTableNameFor base 2) `shouldReturn` 3
                migrationRows conn base
                    `shouldReturn` [(1, Nothing, "bootstrap"), (2, Just "copy", "migration")]

        it
            "blocks an indexed writer during a slow migration and then rejects it, stranding no events" $ \pool -> do
            let base = "test_mig_indexed_writer"
            withTestConn (`dropEventTables` base)
            p <-
                postgresWriteModel pool (TableName base 1) applyTestEvent 0
                    :: IO (PostgresEvent Indexed TestModel TestEvent)
            _ <- runCmd p (Indexed "a") $ \_ -> pure (id, replicate 3 AddOne)
            started <- newEmptyMVar
            let migrated =
                    MigrateWith "slow" (slowMigration @Indexed started) $ TableName base 1
            (writeResult, _) <-
                concurrently
                    ( do
                        readMVar started
                        try @IO @SqlError . runCmd p (Indexed "b") $ \_ -> pure (id, [AddOne])
                    )
                    ( postgresWriteModel pool migrated applyTestEvent 0
                        :: IO (PostgresEvent Indexed TestModel TestEvent)
                    )
            case writeResult of
                Left err -> sqlErrorMsg err `shouldBe` "Event table has been retired."
                Right m -> expectationFailure $ "the indexed write went through during the migration: " <> show m
            withTestConn $ \conn -> do
                countRows conn (eventTableNameFor base 1) `shouldReturn` 3
                countRows conn (eventTableNameFor base 2) `shouldReturn` 3

        it "keeps serving reads during a slow migration" $ \pool -> do
            let base = "test_mig_reads"
            withTestConn (`dropEventTables` base)
            p <- startPersistance pool (TableName base 1)
            addEvents p 3 `shouldReturn` 3
            getModel p NoIndex `shouldReturn` 3
            started <- newEmptyMVar
            let migrated = MigrateWith "slow" (slowMigration @NoIndex started) $ TableName base 1
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
                    (startPersistance pool migrated)
            -- the copy takes >= 0.75s (3 events x 250ms); reads must not wait for it
            readDuration `shouldSatisfy` (< 0.5)

        it "rolls back tables and metadata when a later step of a multi-step chain fails" $ \pool -> do
            let base = "test_mig_rollback"
            withTestConn (`dropEventTables` base)
            p1 <- startPersistance pool (TableName base 1)
            addEvents p1 2 `shouldReturn` 2
            probe <- newIORef (0 :: Int)
            let broken =
                    MigrateWith "boom" (\_ _ _ -> error "ops")
                        . MigrateWith "copy" (probeMigration probe)
                        $ TableName base 1
            startPersistance pool broken `shouldThrow` \(_ :: ErrorCall) -> True
            readIORef probe `shouldReturn` 1
            withTestConn $ \conn -> do
                existingEventTableVersions conn base `shouldReturn` [1]
                migrationRows conn base `shouldReturn` [(1, Nothing, "bootstrap")]
                isRetired conn (eventTableNameFor base 1) `shouldReturn` False
            addEvents p1 1 `shouldReturn` 3

    describe "adopting tables that are not recorded" $ do
        let handBuiltChain :: Connection -> EventTableBaseName -> Bool -> IO ()
            handBuiltChain conn base keepRetiredV1 = do
                dropEventTables conn base
                when keepRetiredV1 $ do
                    void $ createEventTable' conn (eventTableNameFor base 1)
                    retireTable conn (eventTableNameFor base 1)
                void $ createEventTable' conn (eventTableNameFor base 2)
                evs <-
                    traverse
                        (\e -> Stored e (UTCTime (fromGregorian 2020 10 15) 0) <$> mkId)
                        [AddOne, AddOne, AddOne]
                void $ writeEvents conn (eventTableNameFor base 2) NoIndex evs

        it "adopts the live table of a pre-0.7 chain and migrates it further" $ \pool -> do
            let base = "test_mig_adopt"
            withTestConn $ \conn -> handBuiltChain conn base True
            p <- startPersistance pool (MigrateWith "m1" noopMigration $ TableName base 1)
            getModel p NoIndex `shouldReturn` 3
            withTestConn $ \conn ->
                migrationRows conn base `shouldReturn` [(2, Nothing, "adopted")]
            probe <- newIORef (0 :: Int)
            p' <-
                startPersistance pool
                    $ MigrateWith "m2" (probeMigration probe)
                        . MigrateWith "m1" noopMigration
                    $ TableName base 1
            readIORef probe `shouldReturn` 1
            getModel p' NoIndex `shouldReturn` 3
            withTestConn $ \conn -> do
                countRows conn (eventTableNameFor base 3) `shouldReturn` 3
                migrationRows conn base
                    `shouldReturn` [(2, Nothing, "adopted"), (3, Just "m2", "migration")]

        it "adopts the live table when the retired tables below it were dropped" $ \pool -> do
            let base = "test_mig_adopt_gap"
            withTestConn $ \conn -> handBuiltChain conn base False
            p <- startPersistance pool (MigrateWith "m1" noopMigration $ TableName base 1)
            getModel p NoIndex `shouldReturn` 3
            withTestConn $ \conn ->
                migrationRows conn base `shouldReturn` [(2, Nothing, "adopted")]

        it "adopts a migration that pre-0.7 code ran on a recorded chain" $ \pool -> do
            let base = "test_mig_adopt_above"
            withTestConn (`dropEventTables` base)
            p1 <- startPersistance pool (TableName base 1)
            addEvents p1 2 `shouldReturn` 2
            -- what a 0.6 migrator leaves behind: the new table, the old one retired, no row
            withTestConn $ \conn -> withTransaction conn $ do
                void $ createEventTable' conn (eventTableNameFor base 2)
                migrate1to1 @NoIndex @Value conn (eventTableNameFor base 1) (eventTableNameFor base 2) id
                retireTable conn (eventTableNameFor base 1)
            probe <- newIORef (0 :: Int)
            p2 <- startPersistance pool (MigrateWith "m1" (probeMigration probe) $ TableName base 1)
            readIORef probe `shouldReturn` 0
            getModel p2 NoIndex `shouldReturn` 2
            withTestConn $ \conn ->
                migrationRows conn base
                    `shouldReturn` [(1, Nothing, "bootstrap"), (2, Nothing, "adopted")]

        it "adopts a step that a pre-0.7 migrator completes while it waits for the legacy lock" $ \pool -> do
            let base = "test_mig_adopt_race"
            withTestConn (`dropEventTables` base)
            p1 <- startPersistance pool (TableName base 1)
            addEvents p1 2 `shouldReturn` 2
            probe <- newIORef (0 :: Int)
            p2 <- withTestConn $ \conn -> do
                -- A 0.6 migrator: takes the writer lock of the previous table, then migrates.
                begin conn
                advisoryXactLock conn (writerLockKey (eventTableNameFor base 1) NoIndex)
                migrator <-
                    async $
                        startPersistance pool (MigrateWith "m1" (probeMigration probe) $ TableName base 1)
                -- Wait until the 0.7 migrator is blocked on that lock ...
                let waitForBlockedMigrator :: Int -> IO ()
                    waitForBlockedMigrator 0 = expectationFailure "the migrator never waited for the legacy lock"
                    waitForBlockedMigrator n = do
                        [Only blocked] <-
                            query_ @(Only Bool)
                                conn
                                "select exists (select 1 from pg_locks where locktype = 'advisory' and not granted)"
                        unless blocked $ threadDelay 20000 >> waitForBlockedMigrator (n - 1)
                waitForBlockedMigrator 250
                -- ... then complete the 0.6 step and commit.
                void $ createEventTable' conn (eventTableNameFor base 2)
                migrate1to1 @NoIndex @Value conn (eventTableNameFor base 1) (eventTableNameFor base 2) id
                retireTable conn (eventTableNameFor base 1)
                commit conn
                wait migrator
            readIORef probe `shouldReturn` 0
            getModel p2 NoIndex `shouldReturn` 2
            addEvents p2 1 `shouldReturn` 3
            withTestConn $ \conn -> do
                countRows conn (eventTableNameFor base 2) `shouldReturn` 3
                migrationRows conn base
                    `shouldReturn` [(1, Nothing, "bootstrap"), (2, Nothing, "adopted")]

        it "rejects a stray table above the recorded maximum" $ \pool -> do
            let base = "test_mig_stray"
            withTestConn (`dropEventTables` base)
            _ <- startPersistance pool (TableName base 1)
            withTestConn $ \conn -> void $ createEventTable' conn (eventTableNameFor base 99)
            r <- try @IO @MigrationError (startPersistance pool (TableName base 1))
            case r of
                Left e@(UnrecordedEventTable b name _) -> do
                    b `shouldBe` base
                    name `shouldBe` eventTableNameFor base 99
                    displayException e
                        `shouldSatisfy` L.isInfixOf (eventTableNameFor base 98 <> " does not exist")
                other -> expectationFailure $ "unexpected result: " <> show (void other)

        it "rejects an unrecorded table right above a table that still accepts writes" $ \pool -> do
            let base = "test_mig_stray_next"
            withTestConn (`dropEventTables` base)
            _ <- startPersistance pool (TableName base 1)
            withTestConn $ \conn -> void $ createEventTable' conn (eventTableNameFor base 2)
            startPersistance pool (TableName base 1) `shouldThrow` \case
                UnrecordedEventTable b name reason ->
                    b == base
                        && name == eventTableNameFor base 2
                        && "still accepts writes" `L.isInfixOf` reason
                _ -> False

        it "rejects a table that does not look like an event table" $ \pool -> do
            let base = "test_mig_stray_shape"
            withTestConn $ \conn -> do
                dropEventTables conn base
                void $
                    execute_ conn $
                        "create table " <> quoteIdent (eventTableNameFor base 1) <> " (id int)"
            startPersistance pool (TableName base 1) `shouldThrow` \case
                UnrecordedEventTable b name reason ->
                    b == base
                        && name == eventTableNameFor base 1
                        && "lacks the event table column(s)" `L.isInfixOf` reason
                _ -> False

    describe "naming and metadata" $ do
        it "keeps prefix bases apart (foo vs foo_v2)" $ \pool -> do
            let base = "test_mig_prefix"
                prefixBase = base <> "_v2"
            withTestConn $ \conn -> do
                dropEventTables conn base
                dropEventTables conn prefixBase
            pPrefix <- startPersistance pool (TableName prefixBase 1)
            addEvents pPrefix 1 `shouldReturn` 1
            p <- startPersistance pool (MigrateWith "x" noopMigration $ TableName base 1)
            addEvents p 2 `shouldReturn` 2
            _ <- startPersistance pool (TableName prefixBase 1)
            _ <- startPersistance pool (MigrateWith "x" noopMigration $ TableName base 1)
            withTestConn $ \conn -> do
                existingEventTableVersions conn base `shouldReturn` [2]
                existingEventTableVersions conn prefixBase `shouldReturn` [1]
                migrationRows conn base
                    `shouldReturn` [(1, Nothing, "bootstrap"), (2, Just "x", "bootstrap")]
                migrationRows conn prefixBase `shouldReturn` [(1, Nothing, "bootstrap")]
                dropEventTables conn base
                tableExists conn (eventTableNameFor prefixBase 1) `shouldReturn` True
                tableExists conn (eventTableNameFor base 2) `shouldReturn` False

        it "rejects duplicate tags per base but allows several unknown tags" $ \_ -> withTestConn $ \conn -> do
            let base = "test_mig_metadata"
            dropEventTables conn base
            insertMigrationRow conn base 1 Nothing OriginBootstrap
            insertMigrationRow conn base 2 Nothing OriginAdopted
            insertMigrationRow conn base 3 (Just "t") OriginMigration
            insertMigrationRow conn base 4 (Just "t") OriginMigration
                `shouldThrow` (== "23505") . sqlState
            insertMigrationRow conn base 3 (Just "u") OriginMigration
                `shouldThrow` (== "23505") . sqlState
            migrationRows conn base
                `shouldReturn` [(1, Nothing, "bootstrap"), (2, Nothing, "adopted"), (3, Just "t", "migration")]
            dropEventTables conn base

        it "validates the chain before touching the database" $ \pool -> do
            startPersistance
                pool
                ( MigrateWith "same" noopMigration . MigrateWith "same" noopMigration $
                    TableName "test_mig_invalid" 1
                )
                `shouldThrow` (== DuplicateMigrationTag "test_mig_invalid" "same" [2, 3])
            startPersistance pool (TableName "test_mig_invalid" 0)
                `shouldThrow` (== InvalidEventTableVersion "test_mig_invalid" 0)
            withTestConn $ \conn ->
                existingEventTableVersions conn "test_mig_invalid" `shouldReturn` []
