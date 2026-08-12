module Servant.ReqBodyField.LinkSpec (spec) where

import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Servant.API (Get, JSON, QueryParam, (:>))
import Servant.Links (allLinks, linkURI, safeLink)
import Servant.ReqBodyField (ReqBodyField)
import Test.Hspec
import Prelude

type LinkAPI =
    "users"
        :> ReqBodyField "name" Text
        :> QueryParam "page" Int
        :> Get '[JSON] Int

type LinkEndpoint =
    "users"
        :> QueryParam "page" Int
        :> Get '[JSON] Int

spec :: Spec
spec =
    describe "HasLink" $ do
        it "safeLink ignores request body fields" $ do
            let link =
                    safeLink
                        (Proxy @LinkAPI)
                        (Proxy @LinkEndpoint)
                        (Just 3)
            show (linkURI link) `shouldBe` "users?page=3"

        it "allLinks does not require a body-field argument" $ do
            let generated = allLinks (Proxy @LinkAPI)
            show (linkURI (generated Nothing)) `shouldBe` "users"
