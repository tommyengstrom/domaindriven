module DomainDriven.Persistance.PostgresSnapshotSpec where

import Control.DeepSeq (NFData)
import Control.Exception (bracket)
import Control.Monad (replicateM_, void, when)
import Codec.Serialise (Serialise)
import Control.Concurrent (newChan, readChan, threadDelay, writeChan)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString qualified as BS
import Data.IORef
import Data.List (isInfixOf)
import Data.Maybe (fromJust)
import Data.Pool.Introspection qualified as Pool
import Data.UUID.V4 qualified as UUID
import Database.PostgreSQL.Simple
import DomainDriven.Persistance.Class
import DomainDriven.Persistance.Postgres
import DomainDriven.Persistance.Postgres.Internal (createPostgresPersistance, getEventTableName, writeEvents)
import GHC.Generics (Generic)
import GHC.IO.Unsafe (unsafePerformIO)
import System.Timeout qualified as Timeout
import Test.Hspec
import UnliftIO (concurrently, wait, withAsync)
import Prelude

newtype SnapshotTestModel = SnapshotTestModel Int
    deriving stock (Show, Eq, Generic)
    deriving newtype (FromJSON, ToJSON, NFData, Serialise)

data SnapshotTestEvent = Increment
    deriving (Show, Eq, Generic, FromJSON, ToJSON, NFData)

snapshotEventTable :: EventTable
snapshotEventTable = InitialVersion "snapshot_test_events"

applySnapshotEvent :: SnapshotTestModel -> Stored SnapshotTestEvent -> SnapshotTestModel
applySnapshotEvent (SnapshotTestModel count) _ = SnapshotTestModel (count + 1)

spec :: Spec
spec = do
    describe "PostgreSQL snapshot storage" $ do
        it "is not created by the snapshot-disabled constructor" $
            withCleanDatabase $ \pool -> do
                _ <- postgresWriteModel pool snapshotEventTable applySnapshotEvent (SnapshotTestModel 0)
                    :: IO (PostgresEvent NoIndex SnapshotTestModel SnapshotTestEvent)
                withConnection pool $ \conn -> do
                    [Only exists] <- query_ conn "select to_regclass('public.\"domaindriven-snapshots\"') is not null"
                    exists `shouldBe` False

        it "initializes idempotently and stores only the selected format" $
            withCleanDatabase $ \pool -> do
                let store :: SnapshotStore
                    store = postgresSnapshotStore pool
                void $ concurrently (initializeSnapshotStore store) (initializeSnapshotStore store)
                eventId <- UUID.nextRandom
                storeSnapshot store testKey (SnapshotCheckpoint 1 eventId 1) SnapshotJson "1" `shouldReturn` SnapshotStored
                withConnection pool $ \conn -> do
                    rows <- query_ conn "select snapshot_json is not null, snapshot_cbor is null from public.\"domaindriven-snapshots\"" :: IO [(Bool, Bool)]
                    rows `shouldBe` [(True, True)]

        it "retains newer snapshots and permits equal-checkpoint repair" $
            withCleanDatabase $ \pool -> do
                let store :: SnapshotStore
                    store = postgresSnapshotStore pool
                initializeSnapshotStore store
                eventId <- UUID.nextRandom
                olderId <- UUID.nextRandom
                storeSnapshot store testKey (SnapshotCheckpoint 10 eventId 10) SnapshotJson "10" `shouldReturn` SnapshotStored
                storeSnapshot store testKey (SnapshotCheckpoint 9 olderId 9) SnapshotJson "9" `shouldReturn` NewerSnapshotRetained
                storeSnapshot store testKey (SnapshotCheckpoint 10 eventId 10) SnapshotJson "11" `shouldReturn` SnapshotStored
                loaded <- loadSnapshot store testKey
                fmap storedSnapshotPayload loaded `shouldBe` Just "11"
                fmap storedSnapshotCheckpoint loaded `shouldBe` Just (SnapshotCheckpoint 10 eventId 10)

        it "conditionally deletes only the exact loaded row" $
            withCleanDatabase $ \pool -> do
                let store :: SnapshotStore
                    store = postgresSnapshotStore pool
                initializeSnapshotStore store
                eventId <- UUID.nextRandom
                storeSnapshot store testKey (SnapshotCheckpoint 1 eventId 1) SnapshotJson "1" `shouldReturn` SnapshotStored
                Just old <- loadSnapshot store testKey
                storeSnapshot store testKey (SnapshotCheckpoint 1 eventId 1) SnapshotJson "2" `shouldReturn` SnapshotStored
                deleteSnapshot store testKey old
                fmap storedSnapshotPayload <$> loadSnapshot store testKey `shouldReturn` Just "2"

        it "round trips every byte value and conditionally deletes binary snapshots" $
            withCleanDatabase $ \pool -> do
                let store :: SnapshotStore
                    store = postgresSnapshotStore pool
                    payload :: BS.ByteString
                    payload = BS.pack [0 .. 255]
                    replacement :: BS.ByteString
                    replacement = BS.reverse payload
                initializeSnapshotStore store
                eventId <- UUID.nextRandom
                let checkpoint :: SnapshotCheckpoint
                    checkpoint = SnapshotCheckpoint 1 eventId 1
                storeSnapshot store testKey checkpoint SnapshotCbor payload `shouldReturn` SnapshotStored
                Just old <- loadSnapshot store testKey
                storedSnapshotPayload old `shouldBe` payload
                storedSnapshotCheckpoint old `shouldBe` checkpoint
                storeSnapshot store testKey checkpoint SnapshotCbor replacement `shouldReturn` SnapshotStored
                deleteSnapshot store testKey old
                Just current <- loadSnapshot store testKey
                storedSnapshotPayload current `shouldBe` replacement
                deleteSnapshot store testKey current
                loadSnapshot store testKey `shouldReturn` Nothing

    describe "PostgreSQL snapshot runtime" $ do
        it "snapshots every three relevant events" $
            withCleanDatabase $ \pool -> do
                withSnapshotBackend pool $ \backend -> do
                    runIncrement backend
                    snapshotCount pool `shouldReturn` 0
                    runIncrement backend
                    snapshotCount pool `shouldReturn` 0
                    runIncrement backend
                    snapshotHead pool `shouldReturn` [3]
                    replicateM_ 2 (runIncrement backend)
                    snapshotHead pool `shouldReturn` [3]
                    runIncrement backend
                    snapshotHead pool `shouldReturn` [6]

        it "writes one final snapshot for a large batch and ignores empty commands" $
            withCleanDatabase $ \pool -> do
                withSnapshotBackend pool $ \backend -> do
                    void $ runCmd backend NoIndex $ \_ -> pure (id, [])
                    snapshotCount pool `shouldReturn` 0
                    void $ runCmd backend NoIndex $ \_ -> pure (id, replicate 7 Increment)
                    awaitSnapshots backend
                    snapshotHead pool `shouldReturn` [7]

        it "recovers from a snapshot plus tail without applying the prefix" $
            withCleanDatabase $ \pool -> do
                withSnapshotBackend pool $ \backend -> do
                    replicateM_ 3 (runIncrement backend)
                    stored <- toStored Increment
                    withConnection pool $ \conn -> void $ writeEvents conn (getEventTableName snapshotEventTable) NoIndex [stored]
                    applications <- newIORef 0
                    withConfiguredBackend pool (snapshotConfigFor pool) (countingApply applications) $ \fresh -> do
                        getModel fresh NoIndex `shouldReturn` SnapshotTestModel 4
                        readIORef applications `shouldReturn` 1
                        runIncrement fresh
                        snapshotHead pool `shouldReturn` [3]
                        runIncrement fresh
                        snapshotHead pool `shouldReturn` [6]

        it "cold reconstruction creates a snapshot and a warm cache hit avoids another load" $
            withCleanDatabase $ \pool -> do
                plain <- postgresWriteModel pool snapshotEventTable applySnapshotEvent (SnapshotTestModel 0)
                    :: IO (PostgresEvent NoIndex SnapshotTestModel SnapshotTestEvent)
                replicateM_ 3 (runIncrement plain)
                loads <- newIORef (0 :: Int)
                let baseStore :: SnapshotStore
                    baseStore = postgresSnapshotStore pool
                    countingStore :: SnapshotStore
                    countingStore =
                        baseStore
                            { loadSnapshot = \key -> modifyIORef' loads (+ 1) >> loadSnapshot baseStore key
                            }
                    config :: SnapshotConfig SnapshotTestModel
                    config = (snapshotConfigFor pool){snapshotStore = countingStore}
                withConfiguredBackend pool config applySnapshotEvent $ \fresh -> do
                    getModel fresh NoIndex `shouldReturn` SnapshotTestModel 3
                    awaitSnapshots fresh
                    snapshotHead pool `shouldReturn` [3]
                    getModel fresh NoIndex `shouldReturn` SnapshotTestModel 3
                    readIORef loads `shouldReturn` 1

        it "uses per-index event counts despite global event-number gaps" $
            withCleanDatabase $ \pool -> do
                withConfiguredBackend pool (snapshotConfigFor pool) applySnapshotEvent $ \backend -> do
                    let runAt :: Indexed -> IO ()
                        runAt index = void $ runCmd backend index $ \_ -> pure (id, [Increment])
                    runAt (Indexed "a")
                    runAt (Indexed "b")
                    runAt (Indexed "a")
                    runAt (Indexed "b")
                    snapshotCount pool `shouldReturn` 0
                    runAt (Indexed "a")
                    awaitSnapshots backend
                    withConnection pool $ \conn -> do
                        rows <- query_ conn "select event_index, event_number, event_count from public.\"domaindriven-snapshots\"" :: IO [(String, Int, Int)]
                        rows `shouldBe` [("a", 5, 3)]

        it "stores CBOR only in snapshot_cbor and recovers binary payloads without replaying the prefix" $
            withCleanDatabase $ \pool -> do
                let config :: SnapshotConfig SnapshotTestModel
                    config = (snapshotConfigFor pool){snapshotCodec = serialiseCborSnapshotCodec}
                withConfiguredBackend pool config applySnapshotEvent $ \backend -> do
                    void $ runCmd backend NoIndex $ \_ -> pure (id, replicate 256 Increment)
                    awaitSnapshots backend
                    withConnection pool $ \conn -> do
                        rows <- query_ conn "select snapshot_json is null, snapshot_cbor is not null from public.\"domaindriven-snapshots\"" :: IO [(Bool, Bool)]
                        rows `shouldBe` [(True, True)]
                    applications <- newIORef 0
                    withConfiguredBackend pool config (countingApply applications) $ \fresh -> do
                        getModel fresh NoIndex `shouldReturn` SnapshotTestModel 256
                        readIORef applications `shouldReturn` 0

        it "deletes and rebuilds a snapshot with a dangling checkpoint" $
            withCleanDatabase $ \pool -> do
                checkpoint <- seedThreeEvents pool
                let store :: SnapshotStore
                    store = postgresSnapshotStore pool
                initializeSnapshotStore store
                danglingId <- UUID.nextRandom
                storeSnapshot store runtimeKey (SnapshotCheckpoint 999 danglingId 999) SnapshotJson "999" `shouldReturn` SnapshotStored
                withSnapshotBackend pool $ \fresh -> do
                    getModel fresh NoIndex `shouldReturn` SnapshotTestModel 3
                    awaitSnapshots fresh
                    loaded <- loadSnapshot store runtimeKey
                    fmap storedSnapshotCheckpoint loaded `shouldBe` Just checkpoint

        it "rebuilds a snapshot whose payload uses the wrong format" $
            withCleanDatabase $ \pool -> do
                checkpoint <- seedThreeEvents pool
                let store :: SnapshotStore
                    store = postgresSnapshotStore pool
                initializeSnapshotStore store
                storeSnapshot store runtimeKey checkpoint SnapshotCbor "not-the-configured-format" `shouldReturn` SnapshotStored
                withSnapshotBackend pool $ \fresh -> do
                    getModel fresh NoIndex `shouldReturn` SnapshotTestModel 3
                    awaitSnapshots fresh
                    fmap storedSnapshotFormat <$> loadSnapshot store runtimeKey `shouldReturn` Just SnapshotJson

        it "does not hide committed events when snapshot encoding fails" $
            withCleanDatabase $ \pool -> do
                let failingCodec :: SnapshotCodec SnapshotTestModel
                    failingCodec = customSnapshotCodec (fromJust $ mkSnapshotCodecId "failing-json-v1") SnapshotJson (\_ -> error "encode failed") (const $ Right $ SnapshotTestModel 0)
                    config :: SnapshotConfig SnapshotTestModel
                    config = (snapshotConfigFor pool){snapshotCodec = failingCodec}
                withConfiguredBackend pool config applySnapshotEvent $ \backend -> do
                    result <- runCmd backend NoIndex $ \_ -> pure (id, replicate 3 Increment)
                    result `shouldBe` SnapshotTestModel 3
                    awaitSnapshots backend
                    length <$> getEventList backend NoIndex `shouldReturn` 3

        it "logs an undecodable snapshot before rebuilding it from events" $
            withCleanDatabase $ \pool -> do
                checkpoint <- seedThreeEvents pool
                let store :: SnapshotStore
                    store = postgresSnapshotStore pool
                initializeSnapshotStore store
                storeSnapshot store runtimeKey checkpoint SnapshotJson "\"wrong model\"" `shouldReturn` SnapshotStored
                messages <- newIORef ([] :: [String])
                withSnapshotBackend pool $ \fresh -> do
                    getModel (fresh{logger = \entry -> modifyIORef' messages (show entry :)}) NoIndex `shouldReturn` SnapshotTestModel 3
                    awaitSnapshots fresh
                    logged <- readIORef messages
                    logged `shouldSatisfy` any (\entry -> "SnapshotOperationFailure" `isInfixOf` entry && "reject " `isInfixOf` entry)
                    fmap storedSnapshotPayload <$> loadSnapshot store runtimeKey `shouldReturn` Just "3"

        it "does not hide committed events when snapshot storage times out" $
            withCleanDatabase $ \pool -> do
                let baseStore :: SnapshotStore
                    baseStore = postgresSnapshotStore pool
                    slowStore :: SnapshotStore
                    slowStore = baseStore{storeSnapshot = \_ _ _ _ -> threadDelay 100000 >> pure SnapshotStored}
                    config :: SnapshotConfig SnapshotTestModel
                    config =
                        (snapshotConfigFor pool)
                            { snapshotStore = slowStore
                            , snapshotTimeout = fromJust $ mkSnapshotTimeout 0.001
                            }
                withConfiguredBackend pool config applySnapshotEvent $ \backend -> do
                    result <- runCmd backend NoIndex $ \_ -> pure (id, replicate 3 Increment)
                    result `shouldBe` SnapshotTestModel 3
                    awaitSnapshots backend
                    length <$> getEventList backend NoIndex `shouldReturn` 3

        it "falls back to events when snapshot loading fails" $
            withCleanDatabase $ \pool -> do
                _ <- seedThreeEvents pool
                let baseStore :: SnapshotStore
                    baseStore = postgresSnapshotStore pool
                    failingStore :: SnapshotStore
                    failingStore = baseStore{loadSnapshot = \_ -> fail "load failed"}
                    config :: SnapshotConfig SnapshotTestModel
                    config = (snapshotConfigFor pool){snapshotStore = failingStore}
                withConfiguredBackend pool config applySnapshotEvent $ \fresh -> do
                    getModel fresh NoIndex `shouldReturn` SnapshotTestModel 3
                    awaitSnapshots fresh

        it "keeps cadence when a command commits during a snapshot write" $
            withCleanDatabase $ \pool -> within "snapshot and committed command overlap" $ do
                storeStarted <- newChan
                releaseStore <- newChan
                firstWrite <- newIORef True
                let baseStore :: SnapshotStore
                    baseStore = postgresSnapshotStore pool
                    blockingStore :: SnapshotStore
                    blockingStore =
                        baseStore
                            { storeSnapshot = \key checkpoint format payload -> do
                                shouldBlock <- atomicModifyIORef' firstWrite $ \first -> (False, first)
                                when shouldBlock $ do
                                    writeChan storeStarted ()
                                    readChan releaseStore
                                storeSnapshot baseStore key checkpoint format payload
                            }
                    config :: SnapshotConfig SnapshotTestModel
                    config = (snapshotConfigFor pool){snapshotStore = blockingStore}
                withConfiguredBackend pool config applySnapshotEvent $ \backend -> do
                    replicateM_ 2 (runIncrement backend)
                    void $ runCmd backend NoIndex $ \_ -> pure (id, [Increment])
                    readChan storeStarted
                    void $ runCmd backend NoIndex $ \_ -> pure (id, [Increment])
                    writeChan releaseStore ()
                    awaitSnapshots backend
                    snapshotHead pool `shouldReturn` [3]
                    replicateM_ 2 (runIncrement backend)
                    snapshotHead pool `shouldReturn` [6]

        it "keeps cadence when a snapshot finishes before an overlapping command commits" $
            withCleanDatabase $ \pool -> within "snapshot and command overlap" $ do
                storeStarted <- newChan
                releaseStore <- newChan
                commandStarted <- newChan
                releaseCommand <- newChan
                firstWrite <- newIORef True
                let baseStore :: SnapshotStore
                    baseStore = postgresSnapshotStore pool
                    blockingStore :: SnapshotStore
                    blockingStore =
                        baseStore
                            { storeSnapshot = \key checkpoint format payload -> do
                                shouldBlock <- atomicModifyIORef' firstWrite $ \first -> (False, first)
                                when shouldBlock $ do
                                    writeChan storeStarted ()
                                    readChan releaseStore
                                storeSnapshot baseStore key checkpoint format payload
                            }
                    config :: SnapshotConfig SnapshotTestModel
                    config =
                        (snapshotConfigFor pool)
                            { snapshotStore = blockingStore
                            , snapshotTimeout = fromJust $ mkSnapshotTimeout 30
                            }
                withConfiguredBackend pool config applySnapshotEvent $ \backend -> do
                    replicateM_ 2 (runIncrement backend)
                    withAsync (runIncrement backend) $ \snapshotCommand -> do
                        readChan storeStarted
                        let overlappingCommand :: IO SnapshotTestModel
                            overlappingCommand = runCmd backend NoIndex $ \current -> do
                                current `shouldBe` SnapshotTestModel 3
                                writeChan commandStarted ()
                                readChan releaseCommand
                                pure (id, [Increment])
                        withAsync overlappingCommand $ \command -> do
                            readChan commandStarted
                            writeChan releaseStore ()
                            wait snapshotCommand
                            snapshotHead pool `shouldReturn` [3]
                            writeChan releaseCommand ()
                            wait command `shouldReturn` SnapshotTestModel 4
                    snapshotHead pool `shouldReturn` [3]
                    runIncrement backend
                    snapshotHead pool `shouldReturn` [3]
                    runIncrement backend
                    snapshotHead pool `shouldReturn` [6]

        it "propagates commit failure without publishing a model or snapshot" $
            withCleanDatabase $ \pool -> within "deferred constraint commit failure" $ do
                withSnapshotBackend pool $ \backend -> do
                    getModel backend NoIndex `shouldReturn` SnapshotTestModel 0
                    withConnection pool $ \conn ->
                        void $ execute_ conn "alter table snapshot_test_events_v1 add constraint snapshot_index_unique unique (index) deferrable initially deferred"
                    runCmd backend NoIndex (\_ -> pure (id, replicate 3 Increment))
                        `shouldThrow` (\failure -> sqlState failure == "23505")
                    getEventList backend NoIndex `shouldReturn` []
                    snapshotCount pool `shouldReturn` 0
                    getModel backend NoIndex `shouldReturn` SnapshotTestModel 0
                    runIncrement backend
                    getModel backend NoIndex `shouldReturn` SnapshotTestModel 1
                    length <$> getEventList backend NoIndex `shouldReturn` 1
                    snapshotCount pool `shouldReturn` 0

        it "releases a connection slot when starting a transaction fails" $
            withCleanDatabase $ \pool -> within "failed transaction acquisition" $ do
                withSnapshotBackend pool $ \_ -> do
                    firstConnection <- newIORef True
                    let acquireConnection :: IO Connection
                        acquireConnection = do
                            conn <- mkConnection
                            shouldClose <- atomicModifyIORef' firstConnection $ \first -> (False, first)
                            when shouldClose $ close conn
                            pure conn
                        acquirePool :: IO (Pool.Pool Connection)
                        acquirePool = Pool.newPool $ Pool.setNumStripes (Just 1) $ Pool.defaultPoolConfig acquireConnection close 60 1
                    bracket acquirePool Pool.destroyAllResources $ \smallPool -> do
                        backend <- createPostgresPersistance smallPool (getEventTableName snapshotEventTable) applySnapshotEvent (SnapshotTestModel 0)
                            :: IO (PostgresEvent NoIndex SnapshotTestModel SnapshotTestEvent)
                        getModel backend NoIndex `shouldThrow` anyException
                        getModel backend NoIndex `shouldReturn` SnapshotTestModel 0
  where
    testKey :: SnapshotKey
    testKey = SnapshotKey "some_events_v1" "index" testProjection testRevision (snapshotCodecId $ aesonJsonSnapshotCodec @SnapshotTestModel)

    runtimeKey :: SnapshotKey
    runtimeKey = SnapshotKey (getEventTableName snapshotEventTable) "0" testProjection testRevision (snapshotCodecId $ aesonJsonSnapshotCodec @SnapshotTestModel)

withSnapshotBackend
    :: Pool.Pool Connection
    -> (PostgresEvent NoIndex SnapshotTestModel SnapshotTestEvent -> IO a)
    -> IO a
withSnapshotBackend pool = withConfiguredBackend pool (snapshotConfigFor pool) applySnapshotEvent

withConfiguredBackend
    :: Pool.Pool Connection
    -> SnapshotConfig SnapshotTestModel
    -> (SnapshotTestModel -> Stored SnapshotTestEvent -> SnapshotTestModel)
    -> (PostgresEvent index SnapshotTestModel SnapshotTestEvent -> IO a)
    -> IO a
withConfiguredBackend pool config apply =
    bracket
        (postgresWriteModelWithSnapshots pool snapshotEventTable config apply (SnapshotTestModel 0))
        closeSnapshotWriter

testProjection :: SnapshotProjectionName
testProjection = fromJust $ mkSnapshotProjectionName "snapshot-test"

testRevision :: SnapshotProjectionRevision
testRevision = fromJust $ mkSnapshotProjectionRevision "v1"

snapshotConfigFor :: Pool.Pool Connection -> SnapshotConfig SnapshotTestModel
snapshotConfigFor pool =
    SnapshotConfig
        { snapshotProjectionName = testProjection
        , snapshotProjectionRevision = testRevision
        , snapshotCodec = aesonJsonSnapshotCodec
        , snapshotFrequency = fromJust $ mkEveryNEvents 3
        , snapshotTimeout = defaultSnapshotTimeout
        , snapshotQueueCapacity = defaultSnapshotQueueCapacity
        , snapshotStore = postgresSnapshotStore pool
        }

runIncrement :: PostgresEvent NoIndex SnapshotTestModel SnapshotTestEvent -> IO ()
runIncrement backend = do
    void $ runCmd backend NoIndex $ \_ -> pure (id, [Increment])
    awaitSnapshots backend

awaitSnapshots :: PostgresEvent index model event -> IO ()
awaitSnapshots backend = flushSnapshots defaultSnapshotTimeout backend `shouldReturn` SnapshotFlushCompleted

within :: String -> IO a -> IO a
within description action = Timeout.timeout 10000000 action >>= \case
    Just result -> pure result
    Nothing -> expectationFailure ("Timed out: " <> description) >> fail "test timeout"

countingApply :: IORef Int -> SnapshotTestModel -> Stored SnapshotTestEvent -> SnapshotTestModel
countingApply applications model stored = unsafePerformIO $ do
    modifyIORef' applications (+ 1)
    pure $ applySnapshotEvent model stored
{-# NOINLINE countingApply #-}

snapshotCount :: Pool.Pool Connection -> IO Int
snapshotCount pool = withConnection pool $ \conn -> do
    [Only count] <- query_ conn "select count(*) from public.\"domaindriven-snapshots\""
    pure count

snapshotHead :: Pool.Pool Connection -> IO [Int]
snapshotHead pool = withConnection pool $ \conn -> fmap fromOnly <$> query_ conn "select event_number from public.\"domaindriven-snapshots\""

seedThreeEvents :: Pool.Pool Connection -> IO SnapshotCheckpoint
seedThreeEvents pool = do
    backend <- postgresWriteModel pool snapshotEventTable applySnapshotEvent (SnapshotTestModel 0)
        :: IO (PostgresEvent NoIndex SnapshotTestModel SnapshotTestEvent)
    replicateM_ 3 (runIncrement backend)
    withConnection pool $ \conn -> do
        rows <- query_ conn "select event_number, id from snapshot_test_events_v1 order by event_number desc limit 1"
        case rows of
            [(number, eventId)] -> pure $ SnapshotCheckpoint number eventId 3
            _ -> expectationFailure ("Unexpected checkpoint rows: " <> show rows) >> fail "missing checkpoint"

withConnection :: Pool.Pool Connection -> (Connection -> IO a) -> IO a
withConnection pool action = Pool.withResource pool (action . Pool.resource)

withCleanDatabase :: (Pool.Pool Connection -> IO a) -> IO a
withCleanDatabase action = bracket acquire Pool.destroyAllResources action
  where
    acquire :: IO (Pool.Pool Connection)
    acquire = do
        cleanup
        simplePool mkConnection

    cleanup :: IO ()
    cleanup = bracket mkConnection close $ \conn -> do
        void $ execute_ conn "drop table if exists public.\"domaindriven-snapshots\""
        void $ execute_ conn "drop table if exists snapshot_test_events_v1"

mkConnection :: IO Connection
mkConnection =
    connect
        ConnectInfo
            { connectHost = "localhost"
            , connectPort = 5432
            , connectUser = "postgres"
            , connectPassword = "postgres"
            , connectDatabase = "domaindriven"
            }
