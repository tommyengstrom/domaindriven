module Main (main) where

import Control.Monad (unless)
import Control.Monad.Free (Free (Free, Pure))
import Criterion.Main
    ( Benchmark
    , bench
    , bgroup
    , defaultMain
    , nf
    , nfIO
    )
import Data.Aeson
    ( FromJSON (parseJSON)
    , Object
    , ToJSON (toJSON)
    , Value (Object)
    , eitherDecode
    , encode
    , withObject
    , (.:)
    )
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as LBS
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.TypeLits (KnownNat, Nat, natVal)
import Network.HTTP.Media (MediaType)
import Network.HTTP.Types
    ( hContentType
    , methodPost
    , status200
    , statusCode
    )
import Network.Wai (Application, requestHeaders, requestMethod)
import Network.Wai.Test
    ( SRequest (SRequest)
    , defaultRequest
    , runSession
    , simpleBody
    , simpleStatus
    , srequest
    )
import Servant.API
    ( JSON
    , Post
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
import Servant.Server (Handler, serve)
import Prelude

newtype Fields (fieldCount :: Nat) a = Fields [a]
    deriving (Eq, Show)

instance
    forall fieldCount a.
    (KnownNat fieldCount, ToJSON a) =>
    ToJSON (Fields fieldCount a)
    where
    toJSON (Fields values) =
        Object . KeyMap.fromList $
            zip
                (fieldKeys (Proxy @fieldCount))
                (fmap toJSON values)

instance
    forall fieldCount a.
    (KnownNat fieldCount, FromJSON a) =>
    FromJSON (Fields fieldCount a)
    where
    parseJSON =
        withObject "benchmark fields" $ \objectValue ->
            Fields
                <$> traverse
                    (objectValue .:)
                    (fieldKeys (Proxy @fieldCount))

newtype SparseFields (fieldCount :: Nat) a =
    SparseFields [Maybe a]
    deriving (Eq, Show)

instance
    forall fieldCount a.
    (KnownNat fieldCount, ToJSON a) =>
    ToJSON (SparseFields fieldCount a)
    where
    toJSON (SparseFields values) =
        Object . KeyMap.fromList $
            [ (key, toJSON value)
            | (key, Just value) <-
                zip (fieldKeys (Proxy @fieldCount)) values
            ]

fieldKeys :: KnownNat fieldCount => Proxy fieldCount -> [Key.Key]
fieldKeys proxy =
    fmap (Key.fromString . ("f" <>) . show) [1 .. fieldCount]
  where
    fieldCount :: Int
    fieldCount = fromIntegral (natVal proxy)

type JsonPost = Post '[JSON] Value

type Field1 a =
    ReqBodyField "f1" a
        :> JsonPost

type Field8 a =
    ReqBodyField "f1" a
        :> ReqBodyField "f2" a
        :> ReqBodyField "f3" a
        :> ReqBodyField "f4" a
        :> ReqBodyField "f5" a
        :> ReqBodyField "f6" a
        :> ReqBodyField "f7" a
        :> ReqBodyField "f8" a
        :> JsonPost

type Field32 a =
    ReqBodyField "f1" a
        :> ReqBodyField "f2" a
        :> ReqBodyField "f3" a
        :> ReqBodyField "f4" a
        :> ReqBodyField "f5" a
        :> ReqBodyField "f6" a
        :> ReqBodyField "f7" a
        :> ReqBodyField "f8" a
        :> ReqBodyField "f9" a
        :> ReqBodyField "f10" a
        :> ReqBodyField "f11" a
        :> ReqBodyField "f12" a
        :> ReqBodyField "f13" a
        :> ReqBodyField "f14" a
        :> ReqBodyField "f15" a
        :> ReqBodyField "f16" a
        :> ReqBodyField "f17" a
        :> ReqBodyField "f18" a
        :> ReqBodyField "f19" a
        :> ReqBodyField "f20" a
        :> ReqBodyField "f21" a
        :> ReqBodyField "f22" a
        :> ReqBodyField "f23" a
        :> ReqBodyField "f24" a
        :> ReqBodyField "f25" a
        :> ReqBodyField "f26" a
        :> ReqBodyField "f27" a
        :> ReqBodyField "f28" a
        :> ReqBodyField "f29" a
        :> ReqBodyField "f30" a
        :> ReqBodyField "f31" a
        :> ReqBodyField "f32" a
        :> JsonPost

type BodyAPI body = ReqBody '[JSON] body :> JsonPost

field1Handler :: ToJSON a => a -> Handler Value
field1Handler f1 = pure . toJSON $ Fields @1 [f1]

field8Handler
    :: ToJSON a
    => a
    -> a
    -> a
    -> a
    -> a
    -> a
    -> a
    -> a
    -> Handler Value
field8Handler f1 f2 f3 f4 f5 f6 f7 f8 =
    pure . toJSON $
        Fields @8 [f1, f2, f3, f4, f5, f6, f7, f8]

field32Handler
    :: ToJSON a
    => a
    -> a
    -> a
    -> a
    -> a
    -> a
    -> a
    -> a
    -> a
    -> a
    -> a
    -> a
    -> a
    -> a
    -> a
    -> a
    -> a
    -> a
    -> a
    -> a
    -> a
    -> a
    -> a
    -> a
    -> a
    -> a
    -> a
    -> a
    -> a
    -> a
    -> a
    -> a
    -> Handler Value
field32Handler
    f1
    f2
    f3
    f4
    f5
    f6
    f7
    f8
    f9
    f10
    f11
    f12
    f13
    f14
    f15
    f16
    f17
    f18
    f19
    f20
    f21
    f22
    f23
    f24
    f25
    f26
    f27
    f28
    f29
    f30
    f31
    f32 =
        pure . toJSON $
            Fields
                @32
                [ f1
                , f2
                , f3
                , f4
                , f5
                , f6
                , f7
                , f8
                , f9
                , f10
                , f11
                , f12
                , f13
                , f14
                , f15
                , f16
                , f17
                , f18
                , f19
                , f20
                , f21
                , f22
                , f23
                , f24
                , f25
                , f26
                , f27
                , f28
                , f29
                , f30
                , f31
                , f32
                ]

fieldSmall1App :: Application
fieldSmall1App = serve (Proxy @(Field1 Int)) field1Handler

bodySmall1App :: Application
bodySmall1App =
    serve (Proxy @(BodyAPI (Fields 1 Int))) (pure . toJSON)

fieldSmall8App :: Application
fieldSmall8App = serve (Proxy @(Field8 Int)) field8Handler

bodySmall8App :: Application
bodySmall8App =
    serve (Proxy @(BodyAPI (Fields 8 Int))) (pure . toJSON)

fieldSmall32App :: Application
fieldSmall32App = serve (Proxy @(Field32 Int)) field32Handler

bodySmall32App :: Application
bodySmall32App =
    serve (Proxy @(BodyAPI (Fields 32 Int))) (pure . toJSON)

fieldLarge8App :: Application
fieldLarge8App = serve (Proxy @(Field8 Text)) field8Handler

bodyLarge8App :: Application
bodyLarge8App =
    serve (Proxy @(BodyAPI (Fields 8 Text))) (pure . toJSON)

fieldSmall1Client :: Fields 1 Int -> Free ClientF Value
fieldSmall1Client (Fields [f1]) =
    FreeClient.client (Proxy @(Field1 Int)) f1
fieldSmall1Client _ =
    error "fieldSmall1Client: expected exactly one field"

fieldSmall8Client :: Fields 8 Int -> Free ClientF Value
fieldSmall8Client (Fields [f1, f2, f3, f4, f5, f6, f7, f8]) =
    FreeClient.client (Proxy @(Field8 Int))
        f1
        f2
        f3
        f4
        f5
        f6
        f7
        f8
fieldSmall8Client _ =
    error "fieldSmall8Client: expected exactly eight fields"

fieldLarge8Client :: Fields 8 Text -> Free ClientF Value
fieldLarge8Client (Fields [f1, f2, f3, f4, f5, f6, f7, f8]) =
    FreeClient.client (Proxy @(Field8 Text))
        f1
        f2
        f3
        f4
        f5
        f6
        f7
        f8
fieldLarge8Client _ =
    error "fieldLarge8Client: expected exactly eight fields"

fieldSmall32Client :: Fields 32 Int -> Free ClientF Value
fieldSmall32Client
    ( Fields
            [ f1
                , f2
                , f3
                , f4
                , f5
                , f6
                , f7
                , f8
                , f9
                , f10
                , f11
                , f12
                , f13
                , f14
                , f15
                , f16
                , f17
                , f18
                , f19
                , f20
                , f21
                , f22
                , f23
                , f24
                , f25
                , f26
                , f27
                , f28
                , f29
                , f30
                , f31
                , f32
                ]
        ) =
        FreeClient.client (Proxy @(Field32 Int))
            f1
            f2
            f3
            f4
            f5
            f6
            f7
            f8
            f9
            f10
            f11
            f12
            f13
            f14
            f15
            f16
            f17
            f18
            f19
            f20
            f21
            f22
            f23
            f24
            f25
            f26
            f27
            f28
            f29
            f30
            f31
            f32
fieldSmall32Client _ =
    error "fieldSmall32Client: expected exactly 32 fields"

optionalField32Client
    :: SparseFields 32 Int
    -> Free ClientF Value
optionalField32Client
    ( SparseFields
            [ f1
                , f2
                , f3
                , f4
                , f5
                , f6
                , f7
                , f8
                , f9
                , f10
                , f11
                , f12
                , f13
                , f14
                , f15
                , f16
                , f17
                , f18
                , f19
                , f20
                , f21
                , f22
                , f23
                , f24
                , f25
                , f26
                , f27
                , f28
                , f29
                , f30
                , f31
                , f32
                ]
        ) =
        FreeClient.client (Proxy @(Field32 (Maybe Int)))
            f1
            f2
            f3
            f4
            f5
            f6
            f7
            f8
            f9
            f10
            f11
            f12
            f13
            f14
            f15
            f16
            f17
            f18
            f19
            f20
            f21
            f22
            f23
            f24
            f25
            f26
            f27
            f28
            f29
            f30
            f31
            f32
optionalField32Client _ =
    error "optionalField32Client: expected exactly 32 fields"

bodySmall1Client :: Fields 1 Int -> Free ClientF Value
bodySmall1Client =
    FreeClient.client (Proxy @(BodyAPI (Fields 1 Int)))

bodySmall8Client :: Fields 8 Int -> Free ClientF Value
bodySmall8Client =
    FreeClient.client (Proxy @(BodyAPI (Fields 8 Int)))

bodySmall32Client :: Fields 32 Int -> Free ClientF Value
bodySmall32Client =
    FreeClient.client (Proxy @(BodyAPI (Fields 32 Int)))

bodyLarge8Client :: Fields 8 Text -> Free ClientF Value
bodyLarge8Client =
    FreeClient.client (Proxy @(BodyAPI (Fields 8 Text)))

bodyAllNothingClient :: SparseFields 32 Int -> Free ClientF Value
bodyAllNothingClient =
    FreeClient.client (Proxy @(BodyAPI (SparseFields 32 Int)))

small1Payload :: Fields 1 Int
small1Payload = Fields [1]

small8Payload :: Fields 8 Int
small8Payload = Fields [1 .. 8]

small32Payload :: Fields 32 Int
small32Payload = Fields [1 .. 32]

large8Payload :: Fields 8 Text
large8Payload = Fields (replicate 8 largeField)
  where
    largeField = Text.replicate (16 * 1024) "x"

allNothingPayload :: SparseFields 32 Int
allNothingPayload = SparseFields (replicate 32 Nothing)

jsonRequest :: ToJSON body => body -> SRequest
jsonRequest body =
    SRequest
        defaultRequest
            { requestMethod = methodPost
            , requestHeaders = [(hContentType, "application/json")]
            }
        (encode body)

serverObservation :: Application -> SRequest -> IO (Int, LBS.ByteString)
serverObservation application request = do
    response <- runSession (srequest request) application
    pure (statusCode (simpleStatus response), simpleBody response)

requestFrom :: Free ClientF a -> Either String Request
requestFrom (Free (RunRequest request _)) = Right request
requestFrom (Free (Throw clientError)) = Left (show clientError)
requestFrom (Pure _) = Left "generated client did not issue a request"

requestParts
    :: Free ClientF a
    -> Either String (LBS.ByteString, MediaType)
requestParts action = do
    request <- requestFrom action
    case Client.requestBody request of
        Just (RequestBodyLBS body, mediaType) -> Right (body, mediaType)
        Just (RequestBodyBS body, mediaType) ->
            Right (LBS.fromStrict body, mediaType)
        Just (RequestBodySource _, _) -> Left "unexpected streaming body"
        Nothing -> Left "request has no body"

clientRequestBody :: Free ClientF a -> LBS.ByteString
clientRequestBody action =
    case requestParts action of
        Left failure -> error failure
        Right (body, _) -> body

decodeObject :: LBS.ByteString -> Either String Object
decodeObject body =
    case eitherDecode body of
        Right (Object objectValue) -> Right objectValue
        Right _ -> Left "request body was not a JSON object"
        Left parseError -> Left parseError

decodeValue :: LBS.ByteString -> Either String Value
decodeValue = eitherDecode

require :: String -> Bool -> IO ()
require message condition = unless condition (fail message)

validateServerComparison
    :: String
    -> Application
    -> Application
    -> SRequest
    -> IO ()
validateServerComparison name fieldApplication bodyApplication request = do
    (fieldStatus, fieldBody) <- serverObservation fieldApplication request
    (bodyStatus, bodyBody) <- serverObservation bodyApplication request
    require
        (name <> ": ReqBodyField response was not successful")
        (fieldStatus == statusCode status200)
    require
        (name <> ": ReqBody response was not successful")
        (bodyStatus == statusCode status200)
    require
        (name <> ": ReqBodyField returned an empty response body")
        (not (LBS.null fieldBody))
    require
        (name <> ": ReqBody returned an empty response body")
        (not (LBS.null bodyBody))
    case (decodeValue fieldBody, decodeValue bodyBody) of
        (Right fieldValue, Right bodyValue) ->
            require
                (name <> ": server responses were not equivalent")
                (fieldValue == bodyValue)
        (Left parseError, _) ->
            fail (name <> ": invalid ReqBodyField response JSON: " <> parseError)
        (_, Left parseError) ->
            fail (name <> ": invalid ReqBody response JSON: " <> parseError)

validateClientComparison
    :: String
    -> (input -> Free ClientF Value)
    -> (input -> Free ClientF Value)
    -> input
    -> IO ()
validateClientComparison name fieldGenerator bodyGenerator input =
    case
        ( requestParts (fieldGenerator input)
        , requestParts (bodyGenerator input)
        )
    of
        (Right (fieldBody, fieldMediaType), Right (bodyBody, bodyMediaType)) -> do
            require
                (name <> ": ReqBodyField generated an empty request body")
                (not (LBS.null fieldBody))
            require
                (name <> ": ReqBody generated an empty request body")
                (not (LBS.null bodyBody))
            require
                (name <> ": ReqBodyField generated a non-JSON content type")
                (fieldMediaType == jsonMediaType)
            require
                (name <> ": ReqBody generated a non-JSON content type")
                (bodyMediaType == jsonMediaType)
            case (decodeObject fieldBody, decodeObject bodyBody) of
                (Right fieldObject, Right bodyObject) ->
                    require
                        (name <> ": generated JSON objects were not equivalent")
                        (fieldObject == bodyObject)
                (Left parseError, _) ->
                    fail
                        ( name
                            <> ": invalid ReqBodyField request JSON: "
                            <> parseError
                        )
                (_, Left parseError) ->
                    fail
                        (name <> ": invalid ReqBody request JSON: " <> parseError)
        (Left failure, _) ->
            fail (name <> ": ReqBodyField client failed: " <> failure)
        (_, Left failure) ->
            fail (name <> ": ReqBody client failed: " <> failure)

jsonMediaType :: MediaType
jsonMediaType = contentType (Proxy @JSON)

serverComparison
    :: String
    -> Application
    -> Application
    -> SRequest
    -> Benchmark
serverComparison name fieldApplication bodyApplication request =
    bgroup
        name
        [ bench "ReqBodyField" $
            nfIO (serverObservation fieldApplication request)
        , bench "ReqBody" $
            nfIO (serverObservation bodyApplication request)
        ]

clientComparison
    :: String
    -> (input -> Free ClientF Value)
    -> (input -> Free ClientF Value)
    -> input
    -> Benchmark
clientComparison name fieldGenerator bodyGenerator input =
    bgroup
        name
        [ bench "ReqBodyField" $
            nf (clientRequestBody . fieldGenerator) input
        , bench "ReqBody" $
            nf (clientRequestBody . bodyGenerator) input
        ]

main :: IO ()
main = do
    let small1Request = jsonRequest small1Payload
        small8Request = jsonRequest small8Payload
        small32Request = jsonRequest small32Payload
        large8Request = jsonRequest large8Payload

    validateServerComparison
        "server/1-small"
        fieldSmall1App
        bodySmall1App
        small1Request
    validateServerComparison
        "server/8-small"
        fieldSmall8App
        bodySmall8App
        small8Request
    validateServerComparison
        "server/32-small"
        fieldSmall32App
        bodySmall32App
        small32Request
    validateServerComparison
        "server/8-large"
        fieldLarge8App
        bodyLarge8App
        large8Request

    validateClientComparison
        "client/1-small"
        fieldSmall1Client
        bodySmall1Client
        small1Payload
    validateClientComparison
        "client/8-small"
        fieldSmall8Client
        bodySmall8Client
        small8Payload
    validateClientComparison
        "client/32-small"
        fieldSmall32Client
        bodySmall32Client
        small32Payload
    validateClientComparison
        "client/8-large"
        fieldLarge8Client
        bodyLarge8Client
        large8Payload
    validateClientComparison
        "client/32-all-Nothing"
        optionalField32Client
        bodyAllNothingClient
        allNothingPayload

    defaultMain
        [ bgroup
            "server"
            [ serverComparison
                "1-small"
                fieldSmall1App
                bodySmall1App
                small1Request
            , serverComparison
                "8-small"
                fieldSmall8App
                bodySmall8App
                small8Request
            , serverComparison
                "32-small"
                fieldSmall32App
                bodySmall32App
                small32Request
            , serverComparison
                "8-large"
                fieldLarge8App
                bodyLarge8App
                large8Request
            ]
        , bgroup
            "client"
            [ clientComparison
                "1-small"
                fieldSmall1Client
                bodySmall1Client
                small1Payload
            , clientComparison
                "8-small"
                fieldSmall8Client
                bodySmall8Client
                small8Payload
            , clientComparison
                "32-small"
                fieldSmall32Client
                bodySmall32Client
                small32Payload
            , clientComparison
                "8-large"
                fieldLarge8Client
                bodyLarge8Client
                large8Payload
            , clientComparison
                "32-all-Nothing"
                optionalField32Client
                bodyAllNothingClient
                allNothingPayload
            ]
        ]
