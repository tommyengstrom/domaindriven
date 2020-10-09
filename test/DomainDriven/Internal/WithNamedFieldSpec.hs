module DomainDriven.Internal.WithNamedFieldSpec where

import Prelude
import           GHC.Generics
import           Data.Aeson
import           Data.Text                      ( Text )
import DomainDriven.Internal.WithNamedField
import           DomainDriven.Internal.JsonFieldName
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.ADT
import Test.QuickCheck.Classes
import Data.Proxy
import Control.Monad
import Test.Hspec
import Data.String
import Test.Hspec.QuickCheck
import Data.Swagger

data Apa = Apan
    { namedField1 :: Int
    , namedField2 :: Double
    }
    deriving (Show, Eq, Ord, Generic)

instance Arbitrary Apa where
  arbitrary = genericArbitrary
instance FromJSON Apa where
  parseJSON = gParseNamedJson

instance ToJSON Apa where
  toJSON = gToNamedJson

newtype MyText = MyText Text
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (FromJSON, ToJSON, IsString, ToSchema)
    deriving anyclass (JsonFieldName)

instance Arbitrary MyText where
  arbitrary = elements ["hej", "hopp","kalle","anka","ekorre"]

data Bulle = Middag
    | Beer Int MyText
    | Kaffe MyText
    deriving (Show, Eq, Ord, Generic)

instance Arbitrary Bulle where
  arbitrary = genericArbitrary
instance FromJSON Bulle where
  parseJSON = gParseNamedJson

instance ToJSON Bulle where
  toJSON = gToNamedJson

spec :: Spec
spec = describe "Json tests" $ do
    void $ traverse (uncurry prop) (lawsProperties $ jsonLaws $ Proxy @Apa)
    void $ traverse (uncurry prop) (lawsProperties $ jsonLaws $ Proxy @Bulle)
    xit "something something when there are duplicate field names..." False
