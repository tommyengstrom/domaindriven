module Servant.ReqBodyField.OpenApiSpec (spec) where

import Control.Lens ((&), (?~), (^.), (^?), at, ix, _Just)
import Data.Aeson (Value (Null), object, toJSON, (.=))
import Data.OpenApi
    ( AdditionalProperties (AdditionalPropertiesAllowed)
    , NamedSchema (NamedSchema)
    , OpenApi
    , OpenApiType (OpenApiObject, OpenApiString)
    , Operation
    , Reference (Reference)
    , Referenced (Inline, Ref)
    , RequestBody
    , Schema
    , ToSchema (declareNamedSchema)
    , allOf
    , additionalProperties
    , anyOf
    , components
    , content
    , enum_
    , minLength
    , nullable
    , paths
    , post
    , put
    , properties
    , requestBody
    , required
    , schema
    , schemas
    , type_
    )
import Data.Maybe (isJust)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Media (MediaType)
import Servant.API
    ( Description
    , Header
    , JSON
    , Post
    , Put
    , QueryParam
    , contentType
    , (:>)
    , (:<|>)
    )
import Servant.OpenApi (toOpenApi)
import Servant.ReqBodyField (ReqBodyField)
import Test.Hspec
import Prelude

data NamedA = NamedA
    { namedAValue :: Int
    }
    deriving (Generic)

instance ToSchema NamedA

data NamedB = NamedB
    { namedBValue :: Text
    }
    deriving (Generic)

instance ToSchema NamedB

data ConstrainedText

instance ToSchema ConstrainedText where
    declareNamedSchema _ =
        pure $
            NamedSchema Nothing $
                mempty
                    & type_ ?~ OpenApiString
                    & minLength ?~ 3

type MainOpenApi =
    "create"
        :> ReqBodyField "first" Text
        :> Header "X-Interleaved" Text
        :> QueryParam "limit" Int
        :> Description "interleaved"
        :> ReqBodyField "age" Int
        :> ReqBodyField "nickname" (Maybe Text)
        :> Post '[JSON] Int

type OptionalNamedAPI =
    "named"
        :> ReqBodyField "named" (Maybe NamedA)
        :> Post '[JSON] Int

type OptionalConstrainedAPI =
    "constrained"
        :> ReqBodyField "value" (Maybe ConstrainedText)
        :> Post '[JSON] Int

type DuplicateAPI =
    "duplicate"
        :> ReqBodyField "same" NamedA
        :> ReqBodyField "same" NamedB
        :> Post '[JSON] Int

type IdenticalDuplicateAPI =
    "identical"
        :> ReqBodyField "same" NamedA
        :> ReqBodyField "same" NamedA
        :> Post '[JSON] Int

type TripleDuplicateAPI =
    "triple"
        :> ReqBodyField "same" NamedA
        :> ReqBodyField "same" NamedB
        :> ReqBodyField "same" NamedA
        :> Post '[JSON] Int

type MixedDuplicateAPI =
    "mixed"
        :> ReqBodyField "same" (Maybe NamedA)
        :> ReqBodyField "same" NamedB
        :> Post '[JSON] Int

type IdenticalOptionalDuplicateAPI =
    "identical-optional"
        :> ReqBodyField "same" (Maybe NamedA)
        :> ReqBodyField "same" (Maybe NamedA)
        :> Post '[JSON] Int

type DistinctOptionalDuplicateAPI =
    "distinct-optional"
        :> ReqBodyField "same" (Maybe NamedA)
        :> ReqBodyField "same" (Maybe NamedB)
        :> Post '[JSON] Int

type AllOptionalAPI =
    "optional"
        :> ReqBodyField "value" (Maybe Text)
        :> Post '[JSON] Int

type AlternativeAPI =
    ( "left"
        :> ReqBodyField "left" Text
        :> Post '[JSON] Int
    )
        :<|> ( "right"
                :> ReqBodyField "right" Int
                :> Post '[JSON] Int
             )

type SamePathAlternativeAPI =
    ( "same-path"
        :> ReqBodyField "postValue" Text
        :> Post '[JSON] Int
    )
        :<|> ( "same-path"
                :> ReqBodyField "putValue" Int
                :> Put '[JSON] Int
             )

spec :: Spec
spec =
    describe "HasOpenApi" $ do
        it "creates one required object body with merged fields" $ do
            let document = toOpenApi (Proxy @MainOpenApi)
            withOperation document "/create" $ \operation ->
                withRequestRoot operation $ \body root -> do
                    body ^. required `shouldBe` Just True
                    root ^. type_ `shouldBe` Just OpenApiObject
                    root ^. additionalProperties
                        `shouldBe` Just (AdditionalPropertiesAllowed True)
                    root ^. required `shouldBe` ["first", "age"]
                    root ^. properties . at "first" `shouldSatisfy` isJust
                    root ^. properties . at "age" `shouldSatisfy` isJust
                    case root ^. properties . at "nickname" of
                        Just nicknameSchema ->
                            shouldBeOptionalSchema
                                nicknameSchema
                                textSchemaReference
                                (object ["type" .= ("string" :: Text)])
                        other ->
                            expectationFailure $
                                "unexpected nickname schema: " <> show other
                    operation ^. at 400 `shouldSatisfy` isJust

        it "keeps named optional schemas in components and preserves their reference" $ do
            let document = toOpenApi (Proxy @OptionalNamedAPI)
            document ^. components . schemas . at "NamedA" `shouldSatisfy` isJust
            withOperation document "/named" $ \operation ->
                withRequestRoot operation $ \body root -> do
                    body ^. required `shouldBe` Just True
                    root ^. required `shouldBe` []
                    case root ^. properties . at "named" of
                        Just namedSchema ->
                            shouldBeOptionalSchema
                                namedSchema
                                (Ref (componentReference "NamedA"))
                                ( object
                                    [ "$ref"
                                        .= ( "#/components/schemas/NamedA"
                                                :: Text
                                           )
                                    ]
                                )
                        other ->
                            expectationFailure $
                                "unexpected named schema: " <> show other

        it "preserves constrained inline optional schemas" $ do
            let document = toOpenApi (Proxy @OptionalConstrainedAPI)
            withOperation document "/constrained" $ \operation ->
                withRequestRoot operation $ \_ root ->
                    case root ^. properties . at "value" of
                        Just constrainedSchema ->
                            shouldBeOptionalSchema
                                constrainedSchema
                                constrainedTextSchemaReference
                                ( object
                                    [ "type" .= ("string" :: Text)
                                    , "minLength" .= (3 :: Int)
                                    ]
                                )
                        other ->
                            expectationFailure $
                                "unexpected constrained schema: " <> show other

        it "intersects duplicate schemas with allOf" $ do
            let document = toOpenApi (Proxy @DuplicateAPI)
            document ^. components . schemas . at "NamedA"
                `shouldSatisfy` isJust
            document ^. components . schemas . at "NamedB"
                `shouldSatisfy` isJust
            withOperation document "/duplicate" $ \operation ->
                withRequestRoot operation $ \_ root -> do
                    root ^. required `shouldBe` ["same"]
                    case root ^. properties . at "same" of
                        Just (Inline duplicateSchema) ->
                            duplicateSchema ^. allOf
                                `shouldBe` Just
                                    [ Ref (componentReference "NamedA")
                                    , Ref (componentReference "NamedB")
                                    ]
                        other ->
                            expectationFailure $
                                "unexpected duplicate schema: " <> show other

        it "deduplicates identical allOf entries" $ do
            let document = toOpenApi (Proxy @IdenticalDuplicateAPI)
            withOperation document "/identical" $ \operation ->
                withRequestRoot operation $ \_ root ->
                    case root ^. properties . at "same" of
                        Just (Inline duplicateSchema) ->
                            duplicateSchema ^. allOf
                                `shouldBe` Just
                                    [Ref (componentReference "NamedA")]
                        other ->
                            expectationFailure $
                                "unexpected duplicate schema: " <> show other

        it "keeps repeated duplicate schemas flat and deduplicated" $ do
            let document = toOpenApi (Proxy @TripleDuplicateAPI)
            withOperation document "/triple" $ \operation ->
                withRequestRoot operation $ \_ root ->
                    case root ^. properties . at "same" of
                        Just (Inline duplicateSchema) ->
                            duplicateSchema ^. allOf
                                `shouldBe` Just
                                    [ Ref (componentReference "NamedA")
                                    , Ref (componentReference "NamedB")
                                    ]
                        other ->
                            expectationFailure $
                                "unexpected duplicate schema: " <> show other

        it "makes a mixed optional and required duplicate required" $ do
            let document = toOpenApi (Proxy @MixedDuplicateAPI)
            withOperation document "/mixed" $ \operation ->
                withRequestRoot operation $ \_ root -> do
                    root ^. required `shouldBe` ["same"]
                    case root ^. properties . at "same" of
                        Just (Inline duplicateSchema) ->
                            duplicateSchema ^. allOf
                                `shouldBe` Just
                                    [ optionalSchemaReference $
                                        Ref (componentReference "NamedA")
                                    , Ref (componentReference "NamedB")
                                    ]
                        other ->
                            expectationFailure $
                                "unexpected duplicate schema: " <> show other

        it "deduplicates identical optional schemas inside an outer allOf" $ do
            let document = toOpenApi (Proxy @IdenticalOptionalDuplicateAPI)
            withOperation document "/identical-optional" $ \operation ->
                withRequestRoot operation $ \_ root -> do
                    root ^. required `shouldBe` []
                    case root ^. properties . at "same" of
                        Just (Inline duplicateSchema) ->
                            duplicateSchema ^. allOf
                                `shouldBe` Just
                                    [ optionalSchemaReference $
                                        Ref (componentReference "NamedA")
                                    ]
                        other ->
                            expectationFailure $
                                "unexpected duplicate schema: " <> show other

        it "intersects distinct optional schemas with an outer allOf" $ do
            let document = toOpenApi (Proxy @DistinctOptionalDuplicateAPI)
            withOperation document "/distinct-optional" $ \operation ->
                withRequestRoot operation $ \_ root -> do
                    root ^. required `shouldBe` []
                    case root ^. properties . at "same" of
                        Just (Inline duplicateSchema) ->
                            duplicateSchema ^. allOf
                                `shouldBe` Just
                                    [ optionalSchemaReference $
                                        Ref (componentReference "NamedA")
                                    , optionalSchemaReference $
                                        Ref (componentReference "NamedB")
                                    ]
                        other ->
                            expectationFailure $
                                "unexpected duplicate schema: " <> show other

        it "requires the body when every field is optional" $ do
            let document = toOpenApi (Proxy @AllOptionalAPI)
            withOperation document "/optional" $ \operation ->
                withRequestRoot operation $ \body root -> do
                    body ^. required `shouldBe` Just True
                    root ^. required `shouldBe` []

        it "keeps request schemas isolated between alternatives" $ do
            let document = toOpenApi (Proxy @AlternativeAPI)
            withOperation document "/left" $ \operation ->
                withRequestRoot operation $ \_ root -> do
                    root ^. properties . at "left" `shouldSatisfy` isJust
                    root ^. properties . at "right" `shouldBe` Nothing
            withOperation document "/right" $ \operation ->
                withRequestRoot operation $ \_ root -> do
                    root ^. properties . at "right" `shouldSatisfy` isJust
                    root ^. properties . at "left" `shouldBe` Nothing

        it "keeps request schemas isolated between methods on one path" $ do
            let document = toOpenApi (Proxy @SamePathAlternativeAPI)
            withOperation document "/same-path" $ \operation ->
                withRequestRoot operation $ \_ root -> do
                    root ^. properties . at "postValue" `shouldSatisfy` isJust
                    root ^. properties . at "putValue" `shouldBe` Nothing
            withPutOperation document "/same-path" $ \operation ->
                withRequestRoot operation $ \_ root -> do
                    root ^. properties . at "putValue" `shouldSatisfy` isJust
                    root ^. properties . at "postValue" `shouldBe` Nothing

withOperation
    :: OpenApi
    -> FilePath
    -> (Operation -> Expectation)
    -> Expectation
withOperation document path continuation =
    case document ^? paths . ix path . post . _Just of
        Just operation -> continuation operation
        Nothing -> expectationFailure ("missing POST operation at " <> path)

withPutOperation
    :: OpenApi
    -> FilePath
    -> (Operation -> Expectation)
    -> Expectation
withPutOperation document path continuation =
    case document ^? paths . ix path . put . _Just of
        Just operation -> continuation operation
        Nothing -> expectationFailure ("missing PUT operation at " <> path)

withRequestRoot
    :: Operation
    -> (RequestBody -> Schema -> Expectation)
    -> Expectation
withRequestRoot operation continuation =
    case operation ^. requestBody of
        Just (Inline body) ->
            case body ^. content . at jsonMediaType of
                Just mediaTypeObject ->
                    case mediaTypeObject ^. schema of
                        Just (Inline root) -> continuation body root
                        other ->
                            expectationFailure $
                                "unexpected root schema: " <> show other
                Nothing -> expectationFailure "missing JSON request content"
        other -> expectationFailure $ "unexpected request body: " <> show other

jsonMediaType :: MediaType
jsonMediaType = contentType (Proxy @JSON)

componentReference :: Text -> Reference
componentReference = Reference

textSchemaReference :: Referenced Schema
textSchemaReference =
    Inline (mempty & type_ ?~ OpenApiString)

constrainedTextSchemaReference :: Referenced Schema
constrainedTextSchemaReference =
    Inline $
        mempty
            & type_ ?~ OpenApiString
            & minLength ?~ 3

optionalSchemaReference :: Referenced Schema -> Referenced Schema
optionalSchemaReference reference =
    Inline $
        mempty
            & anyOf ?~ [reference, Inline nullOnlySchema]

nullOnlySchema :: Schema
nullOnlySchema =
    mempty
        & type_ ?~ OpenApiString
        & nullable ?~ True
        & enum_ ?~ [Null]

shouldBeOptionalSchema
    :: Referenced Schema
    -> Referenced Schema
    -> Value
    -> Expectation
shouldBeOptionalSchema actual original originalJSON = do
    actual `shouldBe` optionalSchemaReference original
    toJSON actual
        `shouldBe` object
            [ "anyOf"
                .= [ originalJSON
                   , object
                        [ "type" .= ("string" :: Text)
                        , "nullable" .= True
                        , "enum" .= [Null]
                        ]
                   ]
            ]
