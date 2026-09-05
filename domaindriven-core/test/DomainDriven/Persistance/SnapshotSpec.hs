module DomainDriven.Persistance.SnapshotSpec where

import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData (rnf))
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString qualified as BS
import Data.List (isInfixOf)
import Data.Maybe (fromJust, isJust)
import Data.Time (NominalDiffTime)
import DomainDriven.Persistance.Snapshot
import GHC.Generics (Generic)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, UnicodeString (..), counterexample, ioProperty, (===))
import Prelude

data SnapshotModel = SnapshotModel
    { snapshotCount :: Int
    , snapshotLabel :: String
    }
    deriving (Show, Eq, Generic, FromJSON, ToJSON, NFData, Serialise)

data ForceProbe = ForceProbe ~String
    deriving (Show)

instance NFData ForceProbe where
    rnf (ForceProbe value) = rnf value

spec :: Spec
spec = describe "Snapshot codecs" $ do
    let value :: SnapshotModel
        value = SnapshotModel 42 "forty-two"

    prop "round trips strict Aeson JSON" $ roundTrip aesonJsonSnapshotCodec

    prop "round trips strict serialise CBOR" $ roundTrip serialiseCborSnapshotCodec

    it "rejects malformed JSON and CBOR" $ do
        malformedJson <- decodeSnapshot (aesonJsonSnapshotCodec @SnapshotModel) "{"
        malformedJson `shouldSatisfy` isFailure
        malformedCbor <- decodeSnapshot (serialiseCborSnapshotCodec @SnapshotModel) BS.empty
        malformedCbor `shouldSatisfy` isFailure

    it "rejects trailing CBOR bytes" $ do
        encoded <- encodeSnapshot serialiseCborSnapshotCodec value
        case encoded of
            Left failure -> expectationFailure $ show failure
            Right bytes -> do
                decoded <- decodeSnapshot (serialiseCborSnapshotCodec @SnapshotModel) (bytes <> BS.singleton 0)
                decoded `shouldSatisfy` isFailure

    it "fully forces decoded custom models and reports synchronous exceptions" $ do
        let codec :: SnapshotCodec ForceProbe
            codec = customSnapshotCodec (fromJust $ mkSnapshotCodecId "force-probe-v1") SnapshotJson (const BS.empty) (const $ Right $ ForceProbe $ error "force-probe")
        decoded <- decodeSnapshot codec BS.empty
        decoded `shouldSatisfy` \case
            Left failure -> "force-probe" `isInfixOf` snapshotFailureMessage failure
            Right _ -> False

    it "rejects non-positive frequency, timeout, and queue capacity values" $ do
        mkEveryNEvents 0 `shouldBe` Nothing
        mkEveryNEvents (-1) `shouldBe` Nothing
        mkSnapshotTimeout 0 `shouldBe` Nothing
        mkSnapshotTimeout (-1) `shouldBe` Nothing
        mkSnapshotQueueCapacity 0 `shouldBe` Nothing
        mkSnapshotQueueCapacity (-1) `shouldBe` Nothing
        maximumPendingSnapshots <$> mkSnapshotQueueCapacity 1 `shouldBe` Just 1
        maximumPendingSnapshots defaultSnapshotQueueCapacity `shouldBe` 64

    it "validates snapshot identities and supplies stable built-in codec IDs" $ do
        mkSnapshotProjectionName "" `shouldBe` Nothing
        mkSnapshotProjectionRevision "" `shouldBe` Nothing
        mkSnapshotCodecId "" `shouldBe` Nothing
        mkSnapshotProjectionName "bad\0name" `shouldBe` Nothing
        mkSnapshotProjectionRevision "bad\0revision" `shouldBe` Nothing
        mkSnapshotCodecId "bad\0codec" `shouldBe` Nothing
        snapshotCodecIdText (snapshotCodecId $ aesonJsonSnapshotCodec @SnapshotModel) `shouldBe` "aeson-json-v1"
        snapshotCodecIdText (snapshotCodecId $ serialiseCborSnapshotCodec @SnapshotModel) `shouldBe` "serialise-cbor-v1"

    it "accepts only timeouts that fit a positive microsecond count" $ do
        let largestTimeout :: NominalDiffTime
            largestTimeout = fromInteger (toInteger (maxBound :: Int)) / 1000000
        mkSnapshotTimeout largestTimeout `shouldSatisfy` isJust
        mkSnapshotTimeout (largestTimeout + 0.000001) `shouldBe` Nothing
        mkSnapshotTimeout 0.000000000001 `shouldSatisfy` isJust
  where
    isFailure :: Either SnapshotFailure a -> Bool
    isFailure = either (const True) (const False)

roundTrip :: SnapshotCodec SnapshotModel -> Int -> UnicodeString -> Property
roundTrip codec count (UnicodeString label) = ioProperty $ do
    let value :: SnapshotModel
        value = SnapshotModel count label
    encodeSnapshot codec value >>= \case
        Left failure -> pure $ counterexample (show failure) False
        Right bytes -> do
            decoded <- decodeSnapshot codec bytes
            pure $ decoded === Right value
