module DomainDriven.Persistance.PostgresSnapshotSpec where

import Control.DeepSeq (NFData)
import Control.Exception (bracket)
import Control.Monad (replicateM_, void, when)
import Codec.Serialise (Serialise)
import Control.Concurrent (newChan, readChan, threadDelay, writeChan)
import Data.Aeson (FromJSON, ToJSON)
import Data.IORef
import Data.Maybe (fromJust)
import Data.Pool.Introspection qualified as Pool
import Data.UUID.V4 qualified as UUID
import Database.PostgreSQL.Simple
import DomainDriven.Persistance.Class
import DomainDriven.Persistance.Postgres
import DomainDriven.Persistance.Postgres.Internal (getEventTableName, writeEvents)
import GHC.Generics (Generic)
import GHC.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import UnliftIO (concurrently)
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
                let store = postgresSnapshotStore pool
                void $ concurrently (initializeSnapshotStore store) (initializeSnapshotStore store)
                eventId <- UUID.nextRandom
                storeSnapshot store testKey (SnapshotCheckpoint 1 eventId) SnapshotJson "1" `shouldReturn` SnapshotStored
                withConnection pool $ \conn -> do
                    rows <- query_ conn "select snapshot_json is not null, snapshot_cbor is null from public.\"domaindriven-snapshots\"" :: IO [(Bool, Bool)]
                    rows `shouldBe` [(True, True)]

        it "retains newer snapshots and permits equal-checkpoint repair" $
            withCleanDatabase $ \pool -> do
                let store = postgresSnapshotStore pool
                initializeSnapshotStore store
                eventId <- UUID.nextRandom
                olderId <- UUID.nextRandom
                storeSnapshot store testKey (SnapshotCheckpoint 10 eventId) SnapshotJson "10" `shouldReturn` SnapshotStored
                storeSnapshot store testKey (SnapshotCheckpoint 9 olderId) SnapshotJson "9" `shouldReturn` NewerSnapshotRetained
                storeSnapshot store testKey (SnapshotCheckpoint 10 eventId) SnapshotJson "11" `shouldReturn` SnapshotStored
                loaded <- loadSnapshot store testKey
                fmap storedSnapshotPayload loaded `shouldBe` Just "11"

        it "conditionally deletes only the exact loaded row" $
            withCleanDatabase $ \pool -> do
                let store = postgresSnapshotStore pool
                initializeSnapshotStore store
                eventId <- UUID.nextRandom
                storeSnapshot store testKey (SnapshotCheckpoint 1 eventId) SnapshotJson "1" `shouldReturn` SnapshotStored
                Just old <- loadSnapshot store testKey
                storeSnapshot store testKey (SnapshotCheckpoint 1 eventId) SnapshotJson "2" `shouldReturn` SnapshotStored
                deleteSnapshot store testKey old
                fmap storedSnapshotPayload <$> loadSnapshot store testKey `shouldReturn` Just "2"

    describe "PostgreSQL snapshot runtime" $ do
        it "snapshots every three relevant events" $
            withCleanDatabase $ \pool -> do
                backend <- snapshotBackend pool
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
                backend <- snapshotBackend pool
                void $ runCmd backend NoIndex $ \_ -> pure (id, [])
                snapshotCount pool `shouldReturn` 0
                void $ runCmd backend NoIndex $ \_ -> pure (id, replicate 7 Increment)
                snapshotHead pool `shouldReturn` [7]

        it "recovers from a snapshot plus tail without applying the prefix" $
            withCleanDatabase $ \pool -> do
                backend <- snapshotBackend pool
                replicateM_ 3 (runIncrement backend)
                stored <- toStored Increment
                withConnection pool $ \conn -> void $ writeEvents conn (getEventTableName snapshotEventTable) NoIndex [stored]
                applications <- newIORef 0
                fresh <- postgresWriteModelWithSnapshots pool snapshotEventTable (snapshotConfigFor pool) (countingApply applications) (SnapshotTestModel 0)
                getModel fresh NoIndex `shouldReturn` SnapshotTestModel 4
                readIORef applications `shouldReturn` 1

        it "cold reconstruction creates a snapshot and a warm cache hit avoids another load" $
            withCleanDatabase $ \pool -> do
                plain <- postgresWriteModel pool snapshotEventTable applySnapshotEvent (SnapshotTestModel 0)
                    :: IO (PostgresEvent NoIndex SnapshotTestModel SnapshotTestEvent)
                replicateM_ 3 (runIncrement plain)
                loads <- newIORef (0 :: Int)
                let baseStore = postgresSnapshotStore pool
                    countingStore =
                        baseStore
                            { loadSnapshot = \key -> modifyIORef' loads (+ 1) >> loadSnapshot baseStore key
                            }
                    config = (snapshotConfigFor pool){snapshotStore = countingStore}
                fresh <- postgresWriteModelWithSnapshots pool snapshotEventTable config applySnapshotEvent (SnapshotTestModel 0)
                getModel fresh NoIndex `shouldReturn` SnapshotTestModel 3
                snapshotHead pool `shouldReturn` [3]
                getModel fresh NoIndex `shouldReturn` SnapshotTestModel 3
                readIORef loads `shouldReturn` 1

        it "uses per-index event counts despite global event-number gaps" $
            withCleanDatabase $ \pool -> do
                backend <- postgresWriteModelWithSnapshots pool snapshotEventTable (snapshotConfigFor pool) applySnapshotEvent (SnapshotTestModel 0)
                    :: IO (PostgresEvent Indexed SnapshotTestModel SnapshotTestEvent)
                let runAt index = void $ runCmd backend index $ \_ -> pure (id, [Increment])
                runAt (Indexed "a")
                runAt (Indexed "b")
                runAt (Indexed "a")
                runAt (Indexed "b")
                snapshotCount pool `shouldReturn` 0
                runAt (Indexed "a")
                withConnection pool $ \conn -> do
                    rows <- query_ conn "select event_index, event_number from public.\"domaindriven-snapshots\"" :: IO [(String, Int)]
                    rows `shouldBe` [("a", 5)]

        it "stores CBOR only in snapshot_cbor" $
            withCleanDatabase $ \pool -> do
                let config = (snapshotConfigFor pool){snapshotCodec = serialiseCborSnapshotCodec}
                backend <- postgresWriteModelWithSnapshots pool snapshotEventTable config applySnapshotEvent (SnapshotTestModel 0)
                    :: IO (PostgresEvent NoIndex SnapshotTestModel SnapshotTestEvent)
                replicateM_ 3 (runIncrement backend)
                withConnection pool $ \conn -> do
                    rows <- query_ conn "select snapshot_json is null, snapshot_cbor is not null from public.\"domaindriven-snapshots\"" :: IO [(Bool, Bool)]
                    rows `shouldBe` [(True, True)]

        it "deletes and rebuilds a snapshot with a dangling checkpoint" $
            withCleanDatabase $ \pool -> do
                checkpoint <- seedThreeEvents pool
                let store = postgresSnapshotStore pool
                initializeSnapshotStore store
                danglingId <- UUID.nextRandom
                storeSnapshot store runtimeKey (SnapshotCheckpoint 999 danglingId) SnapshotJson "999" `shouldReturn` SnapshotStored
                fresh <- snapshotBackend pool
                getModel fresh NoIndex `shouldReturn` SnapshotTestModel 3
                loaded <- loadSnapshot store runtimeKey
                fmap storedSnapshotCheckpoint loaded `shouldBe` Just checkpoint

        it "rebuilds a snapshot whose payload uses the wrong format" $
            withCleanDatabase $ \pool -> do
                checkpoint <- seedThreeEvents pool
                let store = postgresSnapshotStore pool
                initializeSnapshotStore store
                storeSnapshot store runtimeKey checkpoint SnapshotCbor "not-the-configured-format" `shouldReturn` SnapshotStored
                fresh <- snapshotBackend pool
                getModel fresh NoIndex `shouldReturn` SnapshotTestModel 3
                fmap storedSnapshotFormat <$> loadSnapshot store runtimeKey `shouldReturn` Just SnapshotJson

        it "does not hide committed events when snapshot encoding fails" $
            withCleanDatabase $ \pool -> do
                let failingCodec = customSnapshotCodec SnapshotJson (\_ -> error "encode failed") (const $ Right $ SnapshotTestModel 0)
                    config = (snapshotConfigFor pool){snapshotCodec = failingCodec}
                backend <- postgresWriteModelWithSnapshots pool snapshotEventTable config applySnapshotEvent (SnapshotTestModel 0)
                    :: IO (PostgresEvent NoIndex SnapshotTestModel SnapshotTestEvent)
                result <- runCmd backend NoIndex $ \_ -> pure (id, replicate 3 Increment)
                result `shouldBe` SnapshotTestModel 3
                length <$> getEventList backend NoIndex `shouldReturn` 3

        it "does not hide committed events when snapshot storage times out" $
            withCleanDatabase $ \pool -> do
                let baseStore = postgresSnapshotStore pool
                    slowStore = baseStore{storeSnapshot = \_ _ _ _ -> threadDelay 100000 >> pure SnapshotStored}
                    config =
                        (snapshotConfigFor pool)
                            { snapshotStore = slowStore
                            , snapshotTimeout = fromJust $ mkSnapshotTimeout 0.001
                            }
                backend <- postgresWriteModelWithSnapshots pool snapshotEventTable config applySnapshotEvent (SnapshotTestModel 0)
                    :: IO (PostgresEvent NoIndex SnapshotTestModel SnapshotTestEvent)
                result <- runCmd backend NoIndex $ \_ -> pure (id, replicate 3 Increment)
                result `shouldBe` SnapshotTestModel 3
                length <$> getEventList backend NoIndex `shouldReturn` 3

        it "falls back to events when snapshot loading fails" $
            withCleanDatabase $ \pool -> do
                _ <- seedThreeEvents pool
                let baseStore = postgresSnapshotStore pool
                    failingStore = baseStore{loadSnapshot = \_ -> fail "load failed"}
                    config = (snapshotConfigFor pool){snapshotStore = failingStore}
                fresh <- postgresWriteModelWithSnapshots pool snapshotEventTable config applySnapshotEvent (SnapshotTestModel 0)
                    :: IO (PostgresEvent NoIndex SnapshotTestModel SnapshotTestEvent)
                getModel fresh NoIndex `shouldReturn` SnapshotTestModel 3

        it "keeps cadence when a command commits during a snapshot write" $
            withCleanDatabase $ \pool -> do
                storeStarted <- newChan
                releaseStore <- newChan
                firstWrite <- newIORef True
                let baseStore = postgresSnapshotStore pool
                    blockingStore =
                        baseStore
                            { storeSnapshot = \key checkpoint format payload -> do
                                shouldBlock <- atomicModifyIORef' firstWrite $ \first -> (False, first)
                                when shouldBlock $ do
                                    writeChan storeStarted ()
                                    readChan releaseStore
                                storeSnapshot baseStore key checkpoint format payload
                            }
                    config = (snapshotConfigFor pool){snapshotStore = blockingStore}
                backend <- postgresWriteModelWithSnapshots pool snapshotEventTable config applySnapshotEvent (SnapshotTestModel 0)
                    :: IO (PostgresEvent NoIndex SnapshotTestModel SnapshotTestEvent)
                replicateM_ 2 (runIncrement backend)
                void $
                    concurrently
                        (runIncrement backend)
                        (readChan storeStarted >> runIncrement backend >> writeChan releaseStore ())
                snapshotHead pool `shouldReturn` [3]
                replicateM_ 2 (runIncrement backend)
                snapshotHead pool `shouldReturn` [6]
  where
    testKey :: SnapshotKey
    testKey = SnapshotKey "some_events_v1" "index"

    runtimeKey :: SnapshotKey
    runtimeKey = SnapshotKey (getEventTableName snapshotEventTable) "0"

snapshotBackend
    :: Pool.Pool Connection
    -> IO (PostgresEvent NoIndex SnapshotTestModel SnapshotTestEvent)
snapshotBackend pool =
    postgresWriteModelWithSnapshots pool snapshotEventTable (snapshotConfigFor pool) applySnapshotEvent (SnapshotTestModel 0)

snapshotConfigFor :: Pool.Pool Connection -> SnapshotConfig SnapshotTestModel
snapshotConfigFor pool =
    SnapshotConfig
        { snapshotCodec = aesonJsonSnapshotCodec
        , snapshotFrequency = fromJust $ mkEveryNEvents 3
        , snapshotTimeout = defaultSnapshotTimeout
        , snapshotStore = postgresSnapshotStore pool
        }

runIncrement :: PostgresEvent NoIndex SnapshotTestModel SnapshotTestEvent -> IO ()
runIncrement backend = void $ runCmd backend NoIndex $ \_ -> pure (id, [Increment])

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
            [(number, eventId)] -> pure $ SnapshotCheckpoint number eventId
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
