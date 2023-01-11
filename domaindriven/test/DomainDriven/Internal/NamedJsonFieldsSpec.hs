{-# LANGUAGE DerivingVia #-}

module DomainDriven.Internal.NamedJsonFieldsSpec where

import Control.Monad
import Data.Aeson
import Data.OpenApi
import Data.Proxy
import Data.String
import Data.Text (Text)
import DomainDriven.Internal.HasFieldName
import DomainDriven.Internal.NamedJsonFields
import GHC.Generics
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.ADT
import Test.QuickCheck.Classes
import Prelude

data Test1 = Test1
    { namedField1 :: Int
    , namedField2 :: Double
    }
    deriving (Show, Eq, Ord, Generic)
    deriving (FromJSON, ToJSON, ToSchema) via (NamedJsonFields Test1)

instance Arbitrary Test1 where
    arbitrary = genericArbitrary

newtype MyText = MyText Text
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (FromJSON, ToJSON, IsString, ToSchema)
    deriving anyclass (HasFieldName)

------------------------------------------------------------------------------

instance Arbitrary Duplicated where
    arbitrary = genericArbitrary

instance Arbitrary MyText where
    arbitrary = elements ["hej", "hopp", "kalle", "anka", "ekorre"]

data Test2
    = Test2a
    | Test2b Int MyText
    | Test2c MyText
    | Test2d String String
    | Test2e (Maybe String)
    deriving (Show, Eq, Ord, Generic)
    deriving (FromJSON, ToJSON, ToSchema) via (NamedJsonFields Test2)

instance Arbitrary Test2 where
    arbitrary = genericArbitrary

data Duplicated
    = Duplicated1 Int Int Int String Int String
    | Duplicated2 Int String Int Double
    deriving (Show, Eq, Generic)
    -- deriving anyclass (FromJSON, ToJSON, ToSchema)
    deriving (FromJSON, ToJSON, ToSchema) via (NamedJsonFields Duplicated)

newtype DuplicatedNoTag = DuplicatedNoTag Duplicated
    deriving stock (Generic)
    deriving newtype (Show, Eq, Arbitrary)

noTagOpts :: NamedJsonOptions
noTagOpts = defaultNamedJsonOptions{skipTagField = True}

instance FromJSON DuplicatedNoTag where
    parseJSON = fmap DuplicatedNoTag . gNamedParseJson noTagOpts

instance ToJSON DuplicatedNoTag where
    toJSON (DuplicatedNoTag a) = gNamedToJson noTagOpts a

instance ToSchema DuplicatedNoTag where
    declareNamedSchema _ = gNamedDeclareNamedSchema noTagOpts (Proxy @Duplicated)

spec :: Spec
spec = do
    describe "ToJSON and FromJSON instances" $ do
        describe "Test1" . void $
            traverse
                (uncurry prop)
                (lawsProperties $ jsonLaws $ Proxy @Test1)
        describe "Test2" . void $
            traverse
                (uncurry prop)
                (lawsProperties $ jsonLaws $ Proxy @Test2)
        describe "Duplicated" . void $
            traverse
                (uncurry prop)
                (lawsProperties $ jsonLaws $ Proxy @Duplicated)
        describe "DuplicatedNoTag" . void $
            traverse
                (uncurry prop)
                (lawsProperties $ jsonLaws $ Proxy @DuplicatedNoTag)
    describe "ToSchema instances" $ do
        prop "Test1" $ \(a :: Test1) -> validateToJSON a == []
        prop "Test2" $ \(a :: Test2) -> validateToJSON a == []
        prop "Duplicated" $ \(a :: Duplicated) -> validateToJSON a == []
