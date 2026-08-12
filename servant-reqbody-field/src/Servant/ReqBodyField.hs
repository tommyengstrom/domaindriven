module Servant.ReqBodyField (ReqBodyField) where

import Control.Applicative ((<|>))
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Exception (evaluate)
import Control.Lens ((&), (%~), (.~), (?~), (^.), at)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
    ( FromJSON
    , Object
    , ToJSON (toJSON)
    , Value (Null, Object)
    , decode
    , decodeStrict
    , encode
    , withObject
    , (.:)
    , (.:?)
    )
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString.Lazy qualified as LBS
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.OpenApi
    ( AdditionalProperties (AdditionalPropertiesAllowed)
    , Definitions
    , OpenApiType (OpenApiObject, OpenApiString)
    , Operation
    , Referenced (Inline, Ref)
    , RequestBody
    , Response
    , Schema
    , ToSchema
    , allOf
    , allOperations
    , anyOf
    , components
    , content
    , declareSchemaRef
    , description
    , enum_
    , nullable
    , properties
    , requestBody
    , required
    , schema
    , schemas
    , type_
    , additionalProperties
    )
import Data.OpenApi.Declare (Declare, runDeclare)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Typeable (typeRep)
import Data.Vault.Lazy qualified as Vault
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Network.HTTP.Media (MediaType)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai
    ( Request
    , requestHeaders
    , strictRequestBody
    , vault
    )
import Servant.API
    ( JSON
    , contentType
    , (:>)
    )
import Servant.API.ContentTypes (AllCTUnrender (canHandleCTypeH))
import Servant.API.TypeLevel (IsElem, IsElem')
import Servant.Client.Core
    ( HasClient (Client, clientWithRoute, hoistClientMonad)
    , setRequestBodyLBS
    )
import Servant.Client.Core qualified as Client
import Servant.Links (HasLink (MkLink, toLink))
import Servant.OpenApi (HasOpenApi (toOpenApi))
import Servant.Server
    ( ErrorFormatters (bodyParserErrorFormatter)
    , HasContextEntry
    , HasServer (ServerT, hoistServerWithContext, route)
    , getContextEntry
    )
import Servant.Server.Internal
    ( DelayedIO
    , MkContextWithErrorFormatter
    , RoutingApplication
    , addBodyCheck
    , delayedFail
    , delayedFailFatal
    , err415
    , mkContextWithErrorFormatter
    , withRequest
    )
import System.IO.Unsafe (unsafePerformIO)

-- | Project a field from a shared JSON object request body.
--
-- All 'ReqBodyField' combinators on an endpoint share one request-local parse.
-- Only 'ReqBodyField' participates in that replay: the first body check buffers
-- and consumes the request stream strictly, then the remaining fields use the
-- cached JSON value. Combining it with @ReqBody@, @StreamBody@, or another
-- combinator that reads the body independently is unsupported. @Raw@ and
-- @RawM@ are safe only when the raw application does not read the exhausted
-- body.
--
-- Upstream middleware must leave the body unread or replace it with a
-- replayable stream. Alternatives with the same path and method cannot select
-- a route by body shape: a failed field check returns a fatal @400@ instead of
-- falling through. Because the complete body is held in memory, deployments
-- should enforce suitable request-size limits and timeouts.
data ReqBodyField (field :: Symbol) a

data FieldPresence = RequiredField | OptionalField

type family PresenceOf a where
    PresenceOf (Maybe a) = 'OptionalField
    PresenceOf a = 'RequiredField

class DecodeField (presence :: FieldPresence) a where
    decodeField :: Proxy presence -> Key.Key -> Object -> Parser a

instance FromJSON a => DecodeField 'RequiredField a where
    decodeField _ key objectValue = objectValue .: key

instance FromJSON a => DecodeField 'OptionalField (Maybe a) where
    decodeField _ key objectValue = objectValue .:? key

class EncodeField (presence :: FieldPresence) a where
    encodeField :: Proxy presence -> a -> Maybe Value

instance ToJSON a => EncodeField 'RequiredField a where
    encodeField _ = Just . toJSON

instance ToJSON a => EncodeField 'OptionalField (Maybe a) where
    encodeField _ = fmap toJSON

class FieldSchema (presence :: FieldPresence) a where
    declareFieldSchema
        :: Proxy presence
        -> Proxy a
        -> Declare (Definitions Schema) (Referenced Schema)
    fieldRequired :: Proxy presence -> Proxy a -> Bool

instance ToSchema a => FieldSchema 'RequiredField a where
    declareFieldSchema _ _ = declareSchemaRef (Proxy @a)
    fieldRequired _ _ = True

instance ToSchema a => FieldSchema 'OptionalField (Maybe a) where
    declareFieldSchema _ _ = optionalReference <$> declareSchemaRef (Proxy @a)
    fieldRequired _ _ = False

optionalReference :: Referenced Schema -> Referenced Schema
optionalReference reference =
    Inline $
        mempty
            & anyOf ?~ [reference, Inline nullOnlySchema]

nullOnlySchema :: Schema
nullOnlySchema =
    mempty
        & type_ ?~ OpenApiString
        & nullable ?~ True
        & enum_ ?~ [Null]

type CachedBody = Either String Value
type BodyCache = MVar (Maybe CachedBody)

{-# NOINLINE bodyCacheKey #-}
bodyCacheKey :: Vault.Key BodyCache
bodyCacheKey = unsafePerformIO Vault.newKey

installBodyCache :: RoutingApplication -> RoutingApplication
installBodyCache application request respond =
    case Vault.lookup bodyCacheKey (vault request) of
        Just _ -> application request respond
        Nothing -> do
            cache <- newMVar Nothing
            let requestWithCache =
                    request
                        { vault =
                            Vault.insert bodyCacheKey cache (vault request)
                        }
            application requestWithCache respond

readBodyValue
    :: (LBS.ByteString -> Either String Value)
    -> Request
    -> BodyCache
    -> IO CachedBody
readBodyValue decoder request cache =
    modifyMVar cache $ \case
        Just cached -> pure (Just cached, cached)
        Nothing -> do
            body <- strictRequestBody request
            decoded <- evaluate (decoder body)
            pure (Just decoded, decoded)

instance
    ( KnownSymbol field
    , DecodeField (PresenceOf a) a
    , HasServer api context
    , HasContextEntry
        (MkContextWithErrorFormatter context)
        ErrorFormatters
    ) =>
    HasServer (ReqBodyField field a :> api) context
    where
    type ServerT (ReqBodyField field a :> api) m =
        a -> ServerT api m

    route _ context subserver =
        fmap installBodyCache $
            route (Proxy @api) context $
                addBodyCheck subserver contentTypeCheck bodyCheck
      where
        formatter =
            bodyParserErrorFormatter $
                getContextEntry (mkContextWithErrorFormatter context)
        combinatorType = typeRep (Proxy @ReqBodyField)
        key = Key.fromString (symbolVal (Proxy @field))

        contentTypeCheck
            :: DelayedIO (LBS.ByteString -> Either String Value)
        contentTypeCheck = withRequest $ \request -> do
            let headerValue =
                    fromMaybe "application/octet-stream" $
                        lookup hContentType (requestHeaders request)
            case canHandleCTypeH
                (Proxy @'[JSON])
                (LBS.fromStrict headerValue)
                :: Maybe (LBS.ByteString -> Either String Value) of
                Nothing -> delayedFail err415
                Just decoder -> pure decoder

        bodyCheck
            :: (LBS.ByteString -> Either String Value)
            -> DelayedIO a
        bodyCheck decoder = withRequest $ \request -> do
            cache <-
                case Vault.lookup bodyCacheKey (vault request) of
                    Just existing -> pure existing
                    Nothing ->
                        error "ReqBodyField: body cache was not installed"
            decodedBody <- liftIO (readBodyValue decoder request cache)
            case decodedBody >>= parseEither
                ( withObject "request body" $
                    decodeField (Proxy @(PresenceOf a)) key
                ) of
                Left parseError ->
                    delayedFailFatal $
                        formatter combinatorType request parseError
                Right fieldValue -> pure fieldValue

    hoistServerWithContext _ contextProxy naturalTransformation server =
        hoistServerWithContext
            (Proxy @api)
            contextProxy
            naturalTransformation
            . server

instance
    ( KnownSymbol field
    , EncodeField (PresenceOf a) a
    , HasClient m api
    ) =>
    HasClient m (ReqBodyField field a :> api)
    where
    type Client m (ReqBodyField field a :> api) =
        a -> Client m api

    clientWithRoute monadProxy _ request value =
        clientWithRoute monadProxy (Proxy @api) $
            mergeClientField
                (Key.fromString (symbolVal (Proxy @field)))
                (encodeField (Proxy @(PresenceOf a)) value)
                request

    hoistClientMonad monadProxy _ naturalTransformation generated value =
        hoistClientMonad
            monadProxy
            (Proxy @api)
            naturalTransformation
            (generated value)

mergeClientField
    :: Key.Key
    -> Maybe Value
    -> Client.Request
    -> Client.Request
mergeClientField _ Nothing request =
    case Client.requestBody request of
        Nothing ->
            setRequestBodyLBS
                (encode (Object mempty))
                (contentType (Proxy @JSON))
                request
        Just _ -> request
mergeClientField key (Just fieldValue) request =
    setRequestBodyLBS
        (encode (Object updatedObject))
        (contentType (Proxy @JSON))
        request
  where
    currentObject = requestObject request
    updatedObject = KeyMap.insert key fieldValue currentObject

requestObject :: Client.Request -> Object
requestObject request =
    case Client.requestBody request of
        Just (Client.RequestBodyLBS body, _) -> decodeObjectValue (decode body)
        Just (Client.RequestBodyBS body, _) -> decodeObjectValue (decodeStrict body)
        Just (Client.RequestBodySource _, _) -> mempty
        Nothing -> mempty
  where
    decodeObjectValue (Just (Object objectValue)) = objectValue
    decodeObjectValue _ = mempty

instance HasLink api => HasLink (ReqBodyField field a :> api) where
    type MkLink (ReqBodyField field a :> api) result = MkLink api result
    toLink toResult _ = toLink toResult (Proxy @api)

type instance IsElem' endpoint (ReqBodyField field a :> api) =
    IsElem endpoint api

instance
    ( KnownSymbol field
    , FieldSchema (PresenceOf a) a
    , HasOpenApi api
    ) =>
    HasOpenApi (ReqBodyField field a :> api)
    where
    toOpenApi _ =
        toOpenApi (Proxy @api)
            & allOperations %~ addFieldToOperation fieldName isRequired fieldReference
            & components . schemas %~ (<> definitions)
      where
        fieldName = Text.pack (symbolVal (Proxy @field))
        isRequired =
            fieldRequired (Proxy @(PresenceOf a)) (Proxy @a)
        (definitions, fieldReference) =
            runDeclare
                ( declareFieldSchema
                    (Proxy @(PresenceOf a))
                    (Proxy @a)
                )
                mempty

addFieldToOperation
    :: Text
    -> Bool
    -> Referenced Schema
    -> Operation
    -> Operation
addFieldToOperation fieldName isRequired fieldReference operation =
    operation
        & requestBody %~ Just . Inline . updateRequestBody
        & at 400 %~ (<|> Just invalidBodyResponse)
  where
    updateRequestBody :: Maybe (Referenced RequestBody) -> RequestBody
    updateRequestBody (Just (Inline existing)) = updateInlineBody existing
    updateRequestBody _ = updateInlineBody mempty

    updateInlineBody :: RequestBody -> RequestBody
    updateInlineBody body =
        body
            & required ?~ True
            & content . at jsonMediaType ?~ updatedMediaType
      where
        existingMediaType =
            fromMaybe mempty (body ^. content . at jsonMediaType)
        updatedMediaType =
            existingMediaType
                & schema ?~ Inline updatedRootSchema
        existingRootSchema =
            case existingMediaType ^. schema of
                Just (Inline rootSchema) -> rootSchema
                _ -> mempty
        updatedRootSchema =
            existingRootSchema
                & type_ ?~ OpenApiObject
                & additionalProperties ?~ AdditionalPropertiesAllowed True
                & properties . at fieldName
                    %~ Just . maybe fieldReference
                        (mergeFieldSchemas fieldReference)
                & required %~ updateRequiredFields

    updateRequiredFields fields
        | isRequired && fieldName `notElem` fields = fieldName : fields
        | otherwise = fields

jsonMediaType :: MediaType
jsonMediaType = contentType (Proxy @JSON)

invalidBodyResponse :: Referenced Response
invalidBodyResponse =
    Inline (mempty & description .~ "Invalid `body`")

mergeFieldSchemas
    :: Referenced Schema
    -> Referenced Schema
    -> Referenced Schema
mergeFieldSchemas newSchema oldSchema =
    Inline $
        mempty
            & allOf
                ?~ nub
                    (flattenGeneratedAllOf newSchema <> flattenGeneratedAllOf oldSchema)

flattenGeneratedAllOf :: Referenced Schema -> [Referenced Schema]
flattenGeneratedAllOf reference@(Ref _) = [reference]
flattenGeneratedAllOf reference@(Inline inlineSchema) =
    case inlineSchema ^. allOf of
        Just schemasInAllOf
            | inlineSchema == (mempty & allOf ?~ schemasInAllOf) ->
                schemasInAllOf
        _ -> [reference]
