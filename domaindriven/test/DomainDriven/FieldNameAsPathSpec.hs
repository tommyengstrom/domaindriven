{-# LANGUAGE NoFieldSelectors #-}

module DomainDriven.FieldNameAsPathSpec (spec) where

import Data.Aeson (Value (..), toJSON)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Proxy (Proxy (..))
import DomainDriven.FieldNameAsPath (ApiTagFromLabel, FieldNameAsPathApi)
import GHC.Generics (Generic)
import Servant.API (Get, JSON)
import Servant.API.Generic ((:-))
import Servant.OpenApi (toOpenApi)
import Test.Hspec
import Prelude

data OpenApiTestApi mode = OpenApiTestApi
    { fetchWidget :: mode :- Get '[JSON] Int
    , listWidgets :: mode :- Get '[JSON] [Int]
    }
    deriving stock (Generic)

instance ApiTagFromLabel OpenApiTestApi

spec :: Spec
spec =
    describe "FieldNameAsPath OpenAPI" $
        it "includes every record field as a path" $ do
            let generatedOpenApi =
                    toJSON $
                        toOpenApi (Proxy @(FieldNameAsPathApi OpenApiTestApi))

            case generatedOpenApi of
                Object document ->
                    case KeyMap.lookup "paths" document of
                        Just (Object generatedPaths) -> do
                            KeyMap.member "/fetchWidget" generatedPaths `shouldBe` True
                            KeyMap.member "/listWidgets" generatedPaths `shouldBe` True
                        Just unexpected ->
                            expectationFailure $
                                "expected OpenAPI paths to be an object, but got: "
                                    <> show unexpected
                        Nothing ->
                            expectationFailure "expected generated OpenAPI JSON to contain paths"
                unexpected ->
                    expectationFailure $
                        "expected generated OpenAPI document to be an object, but got: "
                            <> show unexpected
