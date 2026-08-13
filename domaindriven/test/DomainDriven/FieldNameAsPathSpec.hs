{-# LANGUAGE NoFieldSelectors #-}

module DomainDriven.FieldNameAsPathSpec (spec) where

import Control.DeepSeq (NFData)
import Data.Aeson (Value (..), toJSON)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Functor.Identity (Identity (..))
import Data.Proxy (Proxy (..))
import DomainDriven.Aggregate
import DomainDriven.Domain
import DomainDriven.FieldNameAsPath
    ( ApiTagFromLabel
    , FieldNameAsPathApi
    , FieldNameAsPathServer (..)
    , hoistFieldNameAsPathServer
    , zoomProjectionServer
    , zoomProjectionServerI
    , zoomServer
    , zoomServerI
    )
import DomainDriven.Interpreter
import DomainDriven.Persistance.Class (Indexed (..), NoIndex, Stored (..))
import DomainDriven.Persistance.ForgetfulInMemory (createForgetful)
import DomainDriven.Projection
import Effectful
import GHC.Generics (Generic)
import Servant.API (Capture, Get, JSON, Post)
import Servant.API qualified as Servant
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

data NestedApi mode = NestedApi
    { nestedValue :: mode :- Get '[JSON] Int
    , addToNested :: mode :- Capture "amount" Int Servant.:> Get '[JSON] Int
    }
    deriving stock (Generic)

instance ApiTagFromLabel NestedApi

data HoistApi mode = HoistApi
    { nested :: mode :- FieldNameAsPathApi NestedApi
    , echoValue :: mode :- Capture "value" Int Servant.:> Get '[JSON] Int
    }
    deriving stock (Generic)

instance ApiTagFromLabel HoistApi

data ParentModel = ParentModel Int Int
    deriving stock (Show, Eq)

data ChildEvent = Added Int
    deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

data ParentEvent
    = ChildChanged ChildEvent
    | UnrelatedChanged Int
    deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

type ChildDomain = Domain Int ChildEvent NoIndex
type ParentDomain = Domain ParentModel ParentEvent NoIndex

projectChild :: ParentModel -> Int
projectChild (ParentModel childValue _) = childValue

injectChild :: ChildEvent -> ParentEvent
injectChild = ChildChanged

projectChildEvent :: ParentEvent -> Maybe ChildEvent
projectChildEvent = \case
    ChildChanged childEvent -> Just childEvent
    UnrelatedChanged _ -> Nothing

applyParentEvent :: ParentModel -> Stored ParentEvent -> ParentModel
applyParentEvent (ParentModel childValue unrelatedValue) stored =
    case storedEvent stored of
        ChildChanged (Added amount) ->
            ParentModel (childValue + amount) unrelatedValue
        UnrelatedChanged amount ->
            ParentModel childValue (unrelatedValue + amount)

data ReadWriteApi mode = ReadWriteApi
    { readChild :: mode :- Get '[JSON] Int
    , addToChild :: mode :- Capture "amount" Int Servant.:> Post '[JSON] Int
    , readChildHistory :: mode :- Get '[JSON] [Int]
    }
    deriving stock (Generic)

instance ApiTagFromLabel ReadWriteApi

data ReadOnlyApi mode = ReadOnlyApi
    { readOnlyChild :: mode :- Get '[JSON] Int
    , readOnlyHistory :: mode :- Get '[JSON] [Int]
    }
    deriving stock (Generic)

instance ApiTagFromLabel ReadOnlyApi

childEventAmount :: Stored ChildEvent -> Int
childEventAmount stored = case storedEvent stored of
    Added amount -> amount

type IndexedChildDomain = Domain Int ChildEvent Indexed
type IndexedParentDomain = Domain ParentModel ParentEvent Indexed

data IndexedReadWriteApi mode = IndexedReadWriteApi
    { readIndexedChild :: mode :- Get '[JSON] Int
    , addToIndexedChild :: mode :- Capture "amount" Int Servant.:> Post '[JSON] Int
    }
    deriving stock (Generic)

instance ApiTagFromLabel IndexedReadWriteApi

data IndexedReadOnlyApi mode = IndexedReadOnlyApi
    { readIndexedOnly :: mode :- Get '[JSON] Int
    }
    deriving stock (Generic)

instance ApiTagFromLabel IndexedReadOnlyApi

data WriteOnlyApi mode = WriteOnlyApi
    { writeOnlyChild :: mode :- Capture "amount" Int Servant.:> Post '[JSON] Int
    }
    deriving stock (Generic)

instance ApiTagFromLabel WriteOnlyApi

spec :: Spec
spec = do
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

    describe "FieldNameAsPath server helpers" $ do
        it "hoists a raw Generic server and adds exactly one wrapper" $ do
            let FieldNameAsPathServer
                    HoistApi
                        { nested =
                            FieldNameAsPathServer
                                NestedApi
                                    { nestedValue = readNested
                                    , addToNested = addNested
                                    }
                        , echoValue = echo
                        } =
                        hoistFieldNameAsPathServer
                            (\value -> pure (runIdentity value))
                            HoistApi
                                { nested =
                                    FieldNameAsPathServer
                                        NestedApi
                                            { nestedValue = pure 41
                                            , addToNested = pure . (+ 1)
                                            }
                                , echoValue = pure
                                }

            readNested `shouldReturn` 41
            addNested 1 `shouldReturn` 2
            echo 7 `shouldReturn` 7

            let generatedOpenApi =
                    toJSON $ toOpenApi (Proxy @(FieldNameAsPathApi HoistApi))
            case generatedOpenApi of
                Object document ->
                    case KeyMap.lookup "paths" document of
                        Just (Object generatedPaths) -> do
                            KeyMap.member "/nested/nestedValue" generatedPaths
                                `shouldBe` True
                            KeyMap.member "/nested/addToNested/{amount}" generatedPaths
                                `shouldBe` True
                            KeyMap.member "/echoValue/{value}" generatedPaths
                                `shouldBe` True
                        _ -> expectationFailure "expected nested OpenAPI paths"
                _ -> expectationFailure "expected nested OpenAPI document"

        it "runs a read/write child server through zoomServer" $ do
            backend <- createForgetful applyParentEvent (ParentModel 0 100)
            let FieldNameAsPathServer
                    ReadWriteApi
                        { readChild = readChildHandler
                        , addToChild = addToChildHandler
                        , readChildHistory = readHistoryHandler
                        } =
                        zoomServer @ChildDomain @ParentDomain
                            projectChild
                            injectChild
                            projectChildEvent
                            ReadWriteApi
                                { readChild = getModel @ChildDomain
                                , addToChild = \amount ->
                                    runTransaction @ChildDomain $ \_ ->
                                        pure (id, [Added amount])
                                , readChildHistory =
                                    fmap
                                        (fmap childEventAmount)
                                        (getEventList @ChildDomain)
                                }

            (initialChild, result, updatedChild, history) <-
                runEff
                    . runProjection backend
                    . runAggregate backend
                    $ do
                        initialChild <- readChildHandler
                        result <- addToChildHandler 3
                        updatedChild <- readChildHandler
                        history <- readHistoryHandler
                        pure (initialChild, result, updatedChild, history)

            (initialChild, result, updatedChild, history) `shouldBe` (0, 3, 3, [3])
            parentModel <-
                runEff
                    . runProjection backend
                    $ getModel @ParentDomain
            parentModel `shouldBe` ParentModel 3 100

        it "runs model and history reads without an Aggregate effect" $ do
            backend <- createForgetful applyParentEvent (ParentModel 0 100)
            runEff
                . runAggregate backend
                $ runTransaction @ParentDomain $ \_ ->
                    pure
                        ( const ()
                        , [ ChildChanged (Added 2)
                          , UnrelatedChanged 9
                          , ChildChanged (Added 5)
                          ]
                        )

            let FieldNameAsPathServer
                    ReadOnlyApi
                        { readOnlyChild = readChildHandler
                        , readOnlyHistory = readHistoryHandler
                        } =
                        zoomProjectionServer @ChildDomain @ParentDomain
                            projectChild
                            projectChildEvent
                            ReadOnlyApi
                                { readOnlyChild = getModel @ChildDomain
                                , readOnlyHistory =
                                    fmap
                                        (fmap childEventAmount)
                                        (getEventList @ChildDomain)
                                }

            (childModel, history) <-
                runEff
                    . runProjection backend
                    $ (,) <$> readChildHandler <*> readHistoryHandler

            childModel `shouldBe` 7
            history `shouldBe` [2, 5]

        it "runs the indexed server helper variants" $ do
            backend <- createForgetful applyParentEvent (ParentModel 0 100)
            let FieldNameAsPathServer
                    IndexedReadWriteApi
                        { readIndexedChild = readChildHandler
                        , addToIndexedChild = addToChildHandler
                        } =
                        zoomServerI @IndexedChildDomain @IndexedParentDomain
                            id
                            projectChild
                            injectChild
                            projectChildEvent
                            IndexedReadWriteApi
                                { readIndexedChild =
                                    getModelI @IndexedChildDomain (Indexed "child")
                                , addToIndexedChild = \amount ->
                                    runTransactionI
                                        @IndexedChildDomain
                                        (Indexed "child")
                                        $ \_ -> pure (id, [Added amount])
                                }

            (result, model) <-
                runEff
                    . runProjection backend
                    . runAggregate backend
                    $ (,) <$> addToChildHandler 6 <*> readChildHandler
            (result, model) `shouldBe` (6, 6)

            let FieldNameAsPathServer
                    IndexedReadOnlyApi
                        { readIndexedOnly = readOnlyHandler
                        } =
                        zoomProjectionServerI
                            @IndexedChildDomain
                            @IndexedParentDomain
                            id
                            projectChild
                            projectChildEvent
                            IndexedReadOnlyApi
                                { readIndexedOnly =
                                    getModelI @IndexedChildDomain (Indexed "child")
                                }
            (runEff . runProjection backend $ readOnlyHandler) `shouldReturn` 6

        it "supports aggregate-only composition through the hoist helper" $ do
            backend <- createForgetful applyParentEvent (ParentModel 0 100)
            let FieldNameAsPathServer
                    WriteOnlyApi
                        { writeOnlyChild = writeHandler
                        } =
                        hoistFieldNameAsPathServer
                            ( runSubAggregate @ChildDomain @ParentDomain
                                projectChild
                                injectChild
                            )
                            WriteOnlyApi
                                { writeOnlyChild = \amount ->
                                    runTransaction @ChildDomain $ \_ ->
                                        pure (id, [Added amount])
                                }

            result <- runEff . runAggregate backend $ writeHandler 8
            result `shouldBe` 8
            parentModel <-
                runEff
                    . runProjection backend
                    $ getModel @ParentDomain
            parentModel `shouldBe` ParentModel 8 100
