module Servant.ReqBodyField.ServerSpec (spec) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Aeson
    ( Value (Null)
    , eitherDecode
    , encode
    , object
    , toJSON
    , (.=)
    )
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Foldable (traverse_)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Network.HTTP.Types
    ( Method
    , RequestHeaders
    , hContentType
    , methodGet
    , methodPost
    , methodPut
    , status200
    , status400
    , status401
    , status404
    , status405
    , status415
    )
import Network.Wai (Application, requestHeaders, requestMethod)
import Network.Wai.Test
    ( SRequest (SRequest)
    , SResponse
    , defaultRequest
    , runSession
    , setPath
    , simpleBody
    , simpleStatus
    , srequest
    )
import Servant.API
    ( BasicAuth
    , Description
    , Header
    , JSON
    , Post
    , Put
    , QueryParam
    , (:>)
    , (:<|>) ((:<|>))
    )
import Servant.ReqBodyField (ReqBodyField)
import Servant.Server
    ( BasicAuthCheck (BasicAuthCheck)
    , BasicAuthResult (Unauthorized)
    , Context ((:.), EmptyContext)
    , ErrorFormatter
    , ErrorFormatters (bodyParserErrorFormatter)
    , Handler
    , Server
    , ServerError (errBody)
    , ServerT
    , defaultErrorFormatters
    , err400
    , hoistServer
    , serve
    , serveWithContext
    )
import Test.Hspec
import Prelude

type TestAPI =
    "fields"
        :> ReqBodyField "first" Text
        :> Header "X-Interleaved" Text
        :> QueryParam "q" Int
        :> Description "Fields may be interleaved with metadata combinators"
        :> ReqBodyField "age" Int
        :> ReqBodyField "nickname" (Maybe Text)
        :> Post '[JSON] Value
        :<|> "optional"
            :> ReqBodyField "value" (Maybe Text)
            :> Post '[JSON] Value
        :<|> "duplicate"
            :> ReqBodyField "value" Int
            :> ReqBodyField "value" Double
            :> Post '[JSON] Value
        :<|> "echo"
            :> ReqBodyField "value" Int
            :> Post '[JSON] Value
        :<|> "left"
            :> ReqBodyField "left" Text
            :> Post '[JSON] Value
        :<|> "right"
            :> ReqBodyField "right" Int
            :> Post '[JSON] Value

testServer :: Server TestAPI
testServer =
    fieldsHandler
        :<|> (pure . toJSON)
        :<|> duplicateHandler
        :<|> (pure . toJSON)
        :<|> (pure . toJSON)
        :<|> (pure . toJSON)
  where
    fieldsHandler first headerValue queryValue age nickname =
        pure $
            object
                [ "first" .= first
                , "header" .= headerValue
                , "query" .= queryValue
                , "age" .= age
                , "nickname" .= nickname
                ]
    duplicateHandler intValue doubleValue =
        pure $ object ["int" .= intValue, "double" .= doubleValue]

testApp :: Application
testApp = serve (Proxy @TestAPI) testServer

type AuthAPI =
    "auth"
        :> BasicAuth "reqbody-field" Text
        :> ReqBodyField "value" Int
        :> Post '[JSON] Value

authApp :: Application
authApp =
    serveWithContext
        (Proxy @AuthAPI)
        (rejectAll :. EmptyContext)
        (\_ value -> pure (toJSON value))
  where
    rejectAll :: BasicAuthCheck Text
    rejectAll = BasicAuthCheck (const (pure Unauthorized))

type CustomFormatterAPI =
    "custom"
        :> ReqBodyField "value" Int
        :> Post '[JSON] Value

customFormatter :: ErrorFormatter
customFormatter _ _ parseError =
    err400{errBody = "CUSTOM: " <> LBS8.pack parseError}

customFormatterApp :: Application
customFormatterApp =
    serveWithContext
        (Proxy @CustomFormatterAPI)
        ( formatters :. EmptyContext )
        (pure . toJSON)
  where
    formatters =
        defaultErrorFormatters
            { bodyParserErrorFormatter = customFormatter
            }

type HoistedAPI =
    "hoisted"
        :> ReqBodyField "value" Int
        :> Post '[JSON] Value

hoistedServer :: ServerT HoistedAPI (ReaderT () Handler)
hoistedServer = pure . toJSON

hoistedApp :: Application
hoistedApp =
    serve (Proxy @HoistedAPI) $
        hoistServer
            (Proxy @HoistedAPI)
            (`runReaderT` ())
            hoistedServer

type LeadingFieldAPI =
    ReqBodyField "value" Int
        :> "after"
        :> Post '[JSON] Value

leadingFieldApp :: Application
leadingFieldApp = serve (Proxy @LeadingFieldAPI) (pure . toJSON)

type SamePathAPI =
    ( "choice"
        :> ReqBodyField "postValue" Int
        :> Post '[JSON] Value
    )
        :<|> ( "choice"
                :> ReqBodyField "putValue" Text
                :> Put '[JSON] Value
             )

samePathApp :: Application
samePathApp =
    serve
        (Proxy @SamePathAPI)
        ((pure . toJSON) :<|> (pure . toJSON))

type SameMethodAlternativeAPI =
    ( "body-choice"
        :> ReqBodyField "first" Int
        :> Post '[JSON] Value
    )
        :<|> ( "body-choice"
                :> ReqBodyField "second" Text
                :> Post '[JSON] Value
             )

sameMethodAlternativeApp :: Application
sameMethodAlternativeApp =
    serve
        (Proxy @SameMethodAlternativeAPI)
        ( firstHandler :<|> secondHandler )
  where
    firstHandler value = pure $ object ["first" .= value]
    secondHandler value = pure $ object ["second" .= value]

spec :: Spec
spec = do
    describe "HasServer" $ do
        it "decodes multiple interleaved fields from one object" $ do
            response <-
                perform
                    testApp
                    methodPost
                    "/fields?q=9"
                    [ (hContentType, "application/json")
                    , ("X-Interleaved", "present")
                    ]
                    "{\"extra\":true,\"nickname\":\"Countess\",\"age\":37,\"first\":\"Ada\"}"
            simpleStatus response `shouldBe` status200
            response `shouldHaveJSON`
                object
                    [ "first" .= ("Ada" :: Text)
                    , "header" .= Just ("present" :: Text)
                    , "query" .= Just (9 :: Int)
                    , "age" .= (37 :: Int)
                    , "nickname" .= Just ("Countess" :: Text)
                    ]

        it "maps a missing optional property to Nothing" $ do
            response <- postJSON testApp "/optional" "{}"
            simpleStatus response `shouldBe` status200
            response `shouldHaveJSON` Null

        it "maps an explicit null optional property to Nothing" $ do
            response <- postJSON testApp "/optional" "{\"value\":null}"
            simpleStatus response `shouldBe` status200
            response `shouldHaveJSON` Null

        it "decodes duplicate declarations through every declared type" $ do
            response <- postJSON testApp "/duplicate" "{\"value\":7}"
            simpleStatus response `shouldBe` status200
            response `shouldHaveJSON`
                object ["int" .= (7 :: Int), "double" .= (7 :: Double)]

        it "rejects a missing required property" $ do
            response <- postJSON testApp "/echo" "{}"
            simpleStatus response `shouldBe` status400

        it "rejects an invalid field value" $ do
            response <- postJSON testApp "/echo" "{\"value\":\"wrong\"}"
            simpleStatus response `shouldBe` status400

        it "rejects malformed JSON" $ do
            response <- postJSON testApp "/echo" "{"
            simpleStatus response `shouldBe` status400

        it "rejects an empty body even for an optional field" $ do
            response <- postJSON testApp "/optional" ""
            simpleStatus response `shouldBe` status400

        it "rejects a non-object JSON body" $ do
            response <- postJSON testApp "/echo" "[1,2,3]"
            simpleStatus response `shouldBe` status400

        it "requires Content-Type" $ do
            response <- perform testApp methodPost "/echo" [] "{\"value\":1}"
            simpleStatus response `shouldBe` status415

        it "rejects unsupported Content-Type" $ do
            response <-
                perform
                    testApp
                    methodPost
                    "/echo"
                    [(hContentType, "text/plain")]
                    "{"
            simpleStatus response `shouldBe` status415

        it "preserves method-before-content-type error priority" $ do
            response <- perform testApp methodGet "/echo" [] ""
            simpleStatus response `shouldBe` status405

        it "preserves authentication-before-content-type error priority" $ do
            response <- perform authApp methodPost "/auth" [] "not-json"
            simpleStatus response `shouldBe` status401

        it "uses custom body error formatters" $ do
            fieldResponse <-
                postJSON customFormatterApp "/custom" "{\"value\":false}"
            malformedResponse <- postJSON customFormatterApp "/custom" "{"
            traverse_ (\response -> do
                simpleStatus response `shouldBe` status400
                simpleBody response `shouldSatisfy` LBS8.isPrefixOf "CUSTOM:"
                )
                [fieldResponse, malformedResponse]

        it "supports hoisted servers" $ do
            response <- postJSON hoistedApp "/hoisted" "{\"value\":23}"
            simpleStatus response `shouldBe` status200
            response `shouldHaveJSON` toJSON (23 :: Int)

        it "keeps fields isolated between alternatives" $ do
            leftResponse <- postJSON testApp "/left" "{\"left\":\"ok\"}"
            rightResponse <- postJSON testApp "/right" "{\"right\":41}"
            wrongResponse <- postJSON testApp "/right" "{\"left\":\"leak\"}"
            leftResponse `shouldHaveJSON` toJSON ("ok" :: Text)
            rightResponse `shouldHaveJSON` toJSON (41 :: Int)
            simpleStatus wrongResponse `shouldBe` status400

        it "supports a body field before a path without premature parsing" $ do
            matchingResponse <-
                postJSON leadingFieldApp "/after" "{\"value\":19}"
            missingPathResponse <-
                postJSON leadingFieldApp "/missing" "{"
            matchingResponse `shouldHaveJSON` toJSON (19 :: Int)
            simpleStatus missingPathResponse `shouldBe` status404

        it "does not consume the body in a failed same-path alternative" $ do
            response <-
                perform
                    samePathApp
                    methodPut
                    "/choice"
                    [(hContentType, "application/json")]
                    "{\"putValue\":\"selected\"}"
            simpleStatus response `shouldBe` status200
            response `shouldHaveJSON` toJSON ("selected" :: Text)

        it "does not dispatch same-method alternatives by body shape" $ do
            earlierResponse <-
                postJSON
                    sameMethodAlternativeApp
                    "/body-choice"
                    "{\"first\":17}"
            laterOnlyResponse <-
                postJSON
                    sameMethodAlternativeApp
                    "/body-choice"
                    "{\"second\":\"would-match-later\"}"
            simpleStatus earlierResponse `shouldBe` status200
            earlierResponse `shouldHaveJSON` object ["first" .= (17 :: Int)]
            simpleStatus laterOnlyResponse `shouldBe` status400

        it "isolates cached bodies across concurrent requests" $ do
            let values = [1 .. 40] :: [Int]
            responses <-
                mapConcurrently
                    (\value ->
                        postJSON
                            testApp
                            "/echo"
                            (encode (object ["value" .= value]))
                    )
                    values
            map simpleStatus responses `shouldBe` replicate 40 status200
            traverse_ (uncurry shouldHaveJSON) $
                zip responses (map toJSON values)

postJSON :: Application -> ByteString -> LBS.ByteString -> IO SResponse
postJSON application path =
    perform
        application
        methodPost
        path
        [(hContentType, "application/json")]

perform
    :: Application
    -> Method
    -> ByteString
    -> RequestHeaders
    -> LBS.ByteString
    -> IO SResponse
perform application method path headers body =
    runSession
        ( srequest $
            SRequest
                ( (setPath defaultRequest path)
                    { requestMethod = method
                    , requestHeaders = headers
                    }
                )
                body
        )
        application

shouldHaveJSON :: SResponse -> Value -> Expectation
shouldHaveJSON response expected =
    eitherDecode (simpleBody response) `shouldBe` Right expected

infix 1 `shouldHaveJSON`
