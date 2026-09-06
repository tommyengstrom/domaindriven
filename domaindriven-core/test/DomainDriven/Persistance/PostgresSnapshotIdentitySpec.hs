module DomainDriven.Persistance.PostgresSnapshotIdentitySpec where

import Control.Monad (forM_, void)
import Data.Aeson (eitherDecodeStrict', encode)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.IORef
import Data.Maybe (fromJust)
import Data.UUID.V4 qualified as UUID
import DomainDriven.Persistance.Class
import DomainDriven.Persistance.Postgres
import DomainDriven.Persistance.PostgresSnapshotSpec
    ( SnapshotTestEvent (..)
    , SnapshotTestModel (..)
    , applySnapshotEvent
    , awaitSnapshots
    , countingApply
    , snapshotConfigFor
    , snapshotCount
    , testProjection
    , testRevision
    , withCleanDatabase
    , withConfiguredBackend
    )
import Test.Hspec
import Prelude

spec :: Spec
spec = describe "PostgreSQL snapshot identities" $ do
    it "isolates loads, monotonic writes, and exact deletion by every key dimension" $
        withCleanDatabase $ \pool -> do
            let store :: SnapshotStore
                store = postgresSnapshotStore pool
                original :: SnapshotKey
                original = SnapshotKey "events_v1" "index" testProjection testRevision (snapshotCodecId $ aesonJsonSnapshotCodec @SnapshotTestModel)
                variants :: [SnapshotKey]
                variants =
                    [ original{snapshotEventTableName = "events_v2"}
                    , original{snapshotEventIndex = "another-index"}
                    , original{snapshotKeyProjectionName = fromJust $ mkSnapshotProjectionName "projection-'å'"}
                    , original{snapshotKeyProjectionRevision = fromJust $ mkSnapshotProjectionRevision "v2-'å'"}
                    , original{snapshotKeyCodecId = fromJust $ mkSnapshotCodecId "custom-'å'-v1"}
                    ]
            initializeSnapshotStore store
            eventId <- UUID.nextRandom
            let checkpoint :: SnapshotCheckpoint
                checkpoint = SnapshotCheckpoint 10 eventId 10
            storeSnapshot store original checkpoint SnapshotJson "0" `shouldReturn` SnapshotStored
            forM_ (zip variants [1 :: Int ..]) $ \(key, payload) -> do
                loadSnapshot store key `shouldReturn` Nothing
                storeSnapshot store key checkpoint SnapshotJson (BS.pack $ show payload) `shouldReturn` SnapshotStored
            Just previous <- loadSnapshot store original
            storeSnapshot store original checkpoint SnapshotJson "100" `shouldReturn` SnapshotStored
            deleteSnapshot store original previous
            fmap storedSnapshotPayload <$> loadSnapshot store original `shouldReturn` Just "100"
            Just current <- loadSnapshot store original
            deleteSnapshot store original current
            loadSnapshot store original `shouldReturn` Nothing
            forM_ (zip variants [1 :: Int ..]) $ \(key, payload) -> do
                storeSnapshot store key (SnapshotCheckpoint 9 eventId 9) SnapshotJson "999" `shouldReturn` NewerSnapshotRetained
                fmap storedSnapshotPayload <$> loadSnapshot store key `shouldReturn` Just (BS.pack $ show payload)

    it "allows projections, revisions, and codecs to coexist and supports revision rollback without event migration" $
        withCleanDatabase $ \pool -> do
            let original :: SnapshotConfig SnapshotTestModel
                original = snapshotConfigFor pool
                revised :: SnapshotConfig SnapshotTestModel
                revised = original{snapshotProjectionRevision = fromJust $ mkSnapshotProjectionRevision "v2"}
                renamed :: SnapshotConfig SnapshotTestModel
                renamed = original{snapshotProjectionName = fromJust $ mkSnapshotProjectionName "double-count"}
                cbor :: SnapshotConfig SnapshotTestModel
                cbor = original{snapshotCodec = serialiseCborSnapshotCodec}
                custom :: SnapshotConfig SnapshotTestModel
                custom = original{snapshotCodec = customSnapshotCodec
                    (fromJust $ mkSnapshotCodecId "custom-json-v1")
                    SnapshotJson (LBS.toStrict . encode) eitherDecodeStrict'}
                addTen :: SnapshotTestModel -> Stored SnapshotTestEvent -> SnapshotTestModel
                addTen (SnapshotTestModel count) _ = SnapshotTestModel (count + 10)
                addTwo :: SnapshotTestModel -> Stored SnapshotTestEvent -> SnapshotTestModel
                addTwo (SnapshotTestModel count) _ = SnapshotTestModel (count + 2)
            withConfiguredBackend pool original applySnapshotEvent $ \backend -> do
                void $ runCmd backend NoIndex $ \_ -> pure (id, replicate 3 Increment)
                awaitSnapshots backend
                withConfiguredBackend pool revised addTen $ \next -> do
                    getModel next NoIndex `shouldReturn` SnapshotTestModel 30
                    awaitSnapshots next
                withConfiguredBackend pool renamed addTwo $ \other -> do
                    getModel other NoIndex `shouldReturn` SnapshotTestModel 6
                    awaitSnapshots other
                forM_ [cbor, custom] $ \config ->
                    withConfiguredBackend pool config applySnapshotEvent $ \other -> do
                        getModel other NoIndex `shouldReturn` SnapshotTestModel 3
                        awaitSnapshots other
                snapshotCount pool `shouldReturn` 5
                length <$> getEventList backend NoIndex `shouldReturn` 3
            forM_ [original, cbor, custom] $ \config -> do
                applications <- newIORef 0
                withConfiguredBackend pool config (countingApply applications) $ \restored -> do
                    getModel restored NoIndex `shouldReturn` SnapshotTestModel 3
                    readIORef applications `shouldReturn` 0
                    awaitSnapshots restored
            snapshotCount pool `shouldReturn` 5
