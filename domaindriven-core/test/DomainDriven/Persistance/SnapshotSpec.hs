module DomainDriven.Persistance.SnapshotSpec where

import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData (rnf))
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString qualified as BS
import Data.List (isInfixOf)
import DomainDriven.Persistance.Snapshot
import GHC.Generics (Generic)
import Test.Hspec
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
    let value = SnapshotModel 42 "forty-two"

    it "round trips strict Aeson JSON" $ do
        encoded <- encodeSnapshot aesonJsonSnapshotCodec value
        encoded `shouldSatisfy` either (const False) (not . BS.null)
        decoded <- case encoded of
            Left failure -> expectationFailure (show failure) >> pure (Left failure)
            Right bytes -> decodeSnapshot aesonJsonSnapshotCodec bytes
        decoded `shouldBe` Right value

    it "round trips strict serialise CBOR" $ do
        encoded <- encodeSnapshot serialiseCborSnapshotCodec value
        encoded `shouldSatisfy` either (const False) (not . BS.null)
        decoded <- case encoded of
            Left failure -> expectationFailure (show failure) >> pure (Left failure)
            Right bytes -> decodeSnapshot serialiseCborSnapshotCodec bytes
        decoded `shouldBe` Right value

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
        let codec = customSnapshotCodec SnapshotJson (const BS.empty) (const $ Right $ ForceProbe $ error "force-probe")
        decoded <- decodeSnapshot codec BS.empty
        decoded `shouldSatisfy` \case
            Left failure -> "force-probe" `isInfixOf` snapshotFailureMessage failure
            Right _ -> False

    it "rejects non-positive frequency and timeout values" $ do
        mkEveryNEvents 0 `shouldBe` Nothing
        mkEveryNEvents (-1) `shouldBe` Nothing
        mkSnapshotTimeout 0 `shouldBe` Nothing
        mkSnapshotTimeout (-1) `shouldBe` Nothing
  where
    isFailure :: Either SnapshotFailure a -> Bool
    isFailure = either (const True) (const False)
