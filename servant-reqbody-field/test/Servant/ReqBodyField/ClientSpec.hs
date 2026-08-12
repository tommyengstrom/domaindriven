module Servant.ReqBodyField.ClientSpec (spec) where

import Control.Monad.Free (Free (Free, Pure))
import Data.Aeson
    ( Object
    , Value (Object)
    , eitherDecode
    , toJSON
    )
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as LBS
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Network.HTTP.Media (MediaType)
import Servant.API
    ( Description
    , Header
    , JSON
    , PlainText
    , Post
    , QueryParam
    , ReqBody
    , contentType
    , (:>)
    )
import Servant.Client.Core
    ( Request
    , RequestBody (RequestBodyBS, RequestBodyLBS, RequestBodySource)
    )
import Servant.Client.Core qualified as Client
import Servant.Client.Core.RunClient (ClientF (RunRequest, Throw))
import Servant.Client.Free qualified as FreeClient
import Servant.ReqBodyField (ReqBodyField)
import Test.Hspec
import Prelude

type InterleavedAPI =
    ReqBodyField "first" Text
        :> Header "X-Test" Text
        :> QueryParam "limit" Int
        :> Description "interleaved"
        :> ReqBodyField "last" (Maybe Text)
        :> Post '[JSON] Value

type OptionalAPI =
    ReqBodyField "first" (Maybe Text)
        :> ReqBodyField "second" (Maybe Int)
        :> Post '[JSON] Value

type DuplicateAPI =
    ReqBodyField "same" (Maybe Int)
        :> Header "X-Test" Text
        :> ReqBodyField "same" (Maybe Text)
        :> Post '[JSON] Value

type ExistingBodyAPI =
    ReqBody '[PlainText] Text
        :> ReqBodyField "ignored" (Maybe Text)
        :> Post '[JSON] Value

spec :: Spec
spec =
    describe "HasClient" $ do
        it "builds one JSON object across intervening combinators" $ do
            let generated = FreeClient.client (Proxy @InterleavedAPI)
                action = generated "Ada" (Just "header") (Just 3) (Just "Lovelace")
            case requestFrom action of
                Left failure -> expectationFailure failure
                Right request -> do
                    requestMediaType request `shouldBe` Just jsonMediaType
                    requestObject request
                        `shouldBe` Right
                            ( KeyMap.fromList
                                [ ("first", "Ada")
                                , ("last", "Lovelace")
                                ]
                            )

        it "omits optional Nothing properties but still sends an object" $ do
            let generated = FreeClient.client (Proxy @OptionalAPI)
                action = generated Nothing Nothing
            case requestFrom action of
                Left failure -> expectationFailure failure
                Right request -> do
                    requestMediaType request `shouldBe` Just jsonMediaType
                    requestObject request `shouldBe` Right mempty

        it "uses the last non-Nothing duplicate value" $ do
            let generated = FreeClient.client (Proxy @DuplicateAPI)
                lastWins = generated (Just 1) Nothing (Just "last")
                earlierSurvives = generated (Just 1) Nothing Nothing
            requestObjectFrom lastWins
                `shouldBe` Right (KeyMap.fromList [("same", "last")])
            requestObjectFrom earlierSurvives
                `shouldBe` Right
                    (KeyMap.fromList [("same", toJSON (1 :: Int))])

        it "leaves an existing body and media type untouched for Nothing" $ do
            let generated = FreeClient.client (Proxy @ExistingBodyAPI)
                action = generated "existing plain-text body" Nothing
            case requestFrom action of
                Left failure -> expectationFailure failure
                Right request -> do
                    requestMediaType request `shouldBe` Just plainTextMediaType
                    requestBodyBytes request
                        `shouldBe` Right "existing plain-text body"

jsonMediaType :: MediaType
jsonMediaType = contentType (Proxy @JSON)

plainTextMediaType :: MediaType
plainTextMediaType = contentType (Proxy @PlainText)

requestFrom :: Free ClientF a -> Either String Request
requestFrom (Free (RunRequest request _)) = Right request
requestFrom (Free (Throw clientError)) = Left (show clientError)
requestFrom (Pure _) = Left "generated client did not issue a request"

requestObjectFrom :: Free ClientF a -> Either String Object
requestObjectFrom action = requestFrom action >>= requestObject

requestObject :: Request -> Either String Object
requestObject request =
    case Client.requestBody request of
        Just (RequestBodyLBS body, _) -> decodeBody body
        Just (RequestBodyBS body, _) -> decodeBody (LBS.fromStrict body)
        Just (RequestBodySource _, _) -> Left "unexpected streaming body"
        Nothing -> Left "request has no body"
  where
    decodeBody body =
        case eitherDecode body of
            Right (Object objectValue) -> Right objectValue
            Right _ -> Left "request body was not a JSON object"
            Left parseError -> Left parseError

requestBodyBytes :: Request -> Either String LBS.ByteString
requestBodyBytes request =
    case Client.requestBody request of
        Just (RequestBodyLBS body, _) -> Right body
        Just (RequestBodyBS body, _) -> Right (LBS.fromStrict body)
        Just (RequestBodySource _, _) -> Left "unexpected streaming body"
        Nothing -> Left "request has no body"

requestMediaType :: Request -> Maybe MediaType
requestMediaType request = snd <$> Client.requestBody request
