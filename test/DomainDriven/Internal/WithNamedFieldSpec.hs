{-# LANGUAGE DerivingVia #-}
module DomainDriven.Internal.WithNamedFieldSpec where

import           Prelude
import           GHC.Generics
import           Data.Aeson
import           Data.Text                      ( Text )
import           DomainDriven.Internal.WithNamedField
import           DomainDriven.Internal.JsonFieldName
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.ADT
import           Test.QuickCheck.Classes
import           Data.Proxy
import           Control.Monad
import           Test.Hspec
import           Data.String
import           Test.Hspec.QuickCheck
import           Data.Swagger

data Apa = Apan
    { namedField1 :: Int
    , namedField2 :: Double
    }
    deriving (Show, Eq, Ord, Generic)
    deriving (FromJSON, ToJSON) via (WithNamedField Apa)

instance Arbitrary Apa where
    arbitrary = genericArbitrary

newtype MyText = MyText Text
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (FromJSON, ToJSON, IsString, ToSchema)
    deriving anyclass (JsonFieldName)

------------------------------------------------------------------------------


instance Arbitrary Duplicated where
    arbitrary = genericArbitrary

instance Arbitrary MyText where
    arbitrary = elements ["hej", "hopp", "kalle", "anka", "ekorre"]

data Bulle = Middag
    | Beer Int MyText
    | Kaffe MyText
    deriving (Show, Eq, Ord, Generic)
    deriving (FromJSON, ToJSON) via (WithNamedField Bulle)

instance Arbitrary Bulle where
    arbitrary = genericArbitrary

data Duplicated
    = Duplicated1 Int Int Int String Int String
    | Duplicated2 Int String Int Double
    deriving (Show, Eq, Generic)
    deriving (FromJSON, ToJSON) via (WithNamedField Duplicated)

spec :: Spec
spec = describe "Json tests" $ do
    void $ traverse (uncurry prop) (lawsProperties $ jsonLaws $ Proxy @Apa)
    void $ traverse (uncurry prop) (lawsProperties $ jsonLaws $ Proxy @Bulle)
    void $ traverse (uncurry prop) (lawsProperties $ jsonLaws $ Proxy @Duplicated)
