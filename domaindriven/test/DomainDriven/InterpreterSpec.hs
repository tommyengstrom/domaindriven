module DomainDriven.InterpreterSpec (spec) where

import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.DeepSeq (NFData)
import Control.Exception (SomeException, evaluate, try)
import Data.Either (isLeft)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (mapMaybe)
import DomainDriven
import DomainDriven.Persistance.ForgetfulInMemory
    ( ForgetfulInMemory (..)
    , createForgetful
    )
import Effectful
import GHC.Generics (Generic)
import Test.Hspec
import Prelude

data ChildIndex
    = ChildA
    | ChildAliasA
    | ChildB
    deriving stock (Show, Eq)

data ParentModel = ParentModel
    { childValue :: Int
    , unrelatedValue :: Int
    }
    deriving stock (Show, Eq)

data ChildEvent
    = Added Int
    | ParentWideObserved Int
    deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

data ParentEvent
    = ChildChanged ChildEvent
    | ParentWide Int
    | ParentIgnored Int
    deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

type IndexedChildDomain = Domain Int ChildEvent ChildIndex
type IndexedParentDomain = Domain ParentModel ParentEvent Indexed
type NoIndexChildDomain = Domain Int ChildEvent NoIndex
type NoIndexParentDomain = Domain ParentModel ParentEvent NoIndex

routeChild :: ChildIndex -> Indexed
routeChild = \case
    ChildA -> Indexed "a"
    ChildAliasA -> Indexed "a"
    ChildB -> Indexed "b"

projectChildEvent :: ParentEvent -> Maybe ChildEvent
projectChildEvent = \case
    ChildChanged event -> Just event
    ParentWide value -> Just (ParentWideObserved value)
    ParentIgnored _ -> Nothing

applyChildPayload :: Int -> ChildEvent -> Int
applyChildPayload model = \case
    Added amount -> model + amount
    ParentWideObserved _ -> model

applyParentPayload :: ParentModel -> ParentEvent -> ParentModel
applyParentPayload model = \case
    ChildChanged event ->
        model {childValue = applyChildPayload (childValue model) event}
    ParentWide amount ->
        model {unrelatedValue = unrelatedValue model + amount}
    ParentIgnored amount ->
        model {unrelatedValue = unrelatedValue model + amount}

spec :: Spec
spec = do
    describe "indexed sub-domain interpreters" $ do
        it "routes indexes, aliases deliberate collisions, and isolates parent streams" $ do
            backend <-
                createForgetful
                    (\model event -> applyParentPayload model (storedEvent event))
                    (ParentModel 0 0)

            results <-
                runEff
                    . runProjection backend
                    . runAggregate backend
                    . runSubDomainI @IndexedChildDomain @IndexedParentDomain
                        routeChild
                        childValue
                        ChildChanged
                        projectChildEvent
                    $ do
                        resultA <-
                            runTransactionI @IndexedChildDomain ChildA $ \_ ->
                                pure (id, [Added 1])
                        resultAlias <-
                            runTransactionI @IndexedChildDomain ChildAliasA $ \_ ->
                                pure (id, [Added 2])
                        resultB <-
                            runTransactionI @IndexedChildDomain ChildB $ \_ ->
                                pure (id, [Added 5])
                        modelA <- getModelI @IndexedChildDomain ChildA
                        modelAlias <- getModelI @IndexedChildDomain ChildAliasA
                        modelB <- getModelI @IndexedChildDomain ChildB
                        pure
                            ( resultA
                            , resultAlias
                            , resultB
                            , modelA
                            , modelAlias
                            , modelB
                            )

            results `shouldBe` (1, 3, 5, 3, 3, 5)

        it "projects callback input and updated results while preserving unrelated state" $ do
            backend <-
                createForgetful
                    (\model event -> applyParentPayload model (storedEvent event))
                    (ParentModel 0 0)
            runEff
                . runAggregate backend
                $ runTransactionI @IndexedParentDomain (Indexed "a") $ \_ ->
                    pure (const (), [ChildChanged (Added 2), ParentIgnored 100])

            result <-
                runEff
                    . runAggregate backend
                    . runSubAggregateI @IndexedChildDomain @IndexedParentDomain
                        routeChild
                        childValue
                        ChildChanged
                    $ runTransactionI @IndexedChildDomain ChildA $ \childModel -> do
                        liftIO $ childModel `shouldBe` 2
                        pure (id, [Added 3, Added 4])

            result `shouldBe` 9
            (parentModel, parentEvents) <-
                runEff
                    . runProjection backend
                    $ (,)
                        <$> getModelI @IndexedParentDomain (Indexed "a")
                        <*> getEventListI @IndexedParentDomain (Indexed "a")
            parentModel `shouldBe` ParentModel 9 100
            fmap storedEvent parentEvents
                `shouldBe` [ ChildChanged (Added 2)
                           , ParentIgnored 100
                           , ChildChanged (Added 3)
                           , ChildChanged (Added 4)
                           ]

        it "preserves projected history metadata and retains explicit wide events" $ do
            backend <-
                createForgetful
                    (\model event -> applyParentPayload model (storedEvent event))
                    (ParentModel 0 0)
            runEff . runAggregate backend $ do
                runTransactionI @IndexedParentDomain (Indexed "a") $ \_ ->
                    pure
                        ( const ()
                        , [ ChildChanged (Added 2)
                          , ParentIgnored 7
                          , ParentWide 11
                          , ChildChanged (Added 3)
                          ]
                        )
                runTransactionI @IndexedParentDomain (Indexed "b") $ \_ ->
                    pure (const (), [ChildChanged (Added 99)])

            parentHistory <-
                runEff
                    . runProjection backend
                    $ getEventListI @IndexedParentDomain (Indexed "a")
            childHistory <-
                runEff
                    . runProjection backend
                    . runSubProjectionI @IndexedChildDomain @IndexedParentDomain
                        routeChild
                        childValue
                        projectChildEvent
                    $ getEventListI @IndexedChildDomain ChildA

            childHistory
                `shouldBe` mapMaybe (traverse projectChildEvent) parentHistory
            fmap storedEvent childHistory
                `shouldBe` [Added 2, ParentWideObserved 11, Added 3]

        it "keeps lower-stack effects available inside the child callback" $ do
            backend <-
                createForgetful
                    (\model event -> applyParentPayload model (storedEvent event))
                    (ParentModel 0 0)
            observed <- newIORef Nothing

            result <-
                runEff
                    . runAggregate backend
                    . runSubAggregateI @IndexedChildDomain @IndexedParentDomain
                        routeChild
                        childValue
                        ChildChanged
                    $ runTransactionI @IndexedChildDomain ChildA $ \childModel -> do
                        liftIO $ writeIORef observed (Just childModel)
                        pure (id, [Added 4])

            result `shouldBe` 4
            readIORef observed `shouldReturn` Just 0

        it "forwards one ordered parent batch per child transaction" $ do
            batches <- newChan
            baseBackend <-
                createForgetful
                    (\model event -> applyParentPayload model (storedEvent event))
                    (ParentModel 0 0)
            let backend =
                    baseBackend
                        { updateHook = \index _ events ->
                            writeChan batches (index, fmap storedEvent events)
                        }

            _ <-
                runEff
                    . runAggregate backend
                    . runSubAggregateI @IndexedChildDomain @IndexedParentDomain
                        routeChild
                        childValue
                        ChildChanged
                    $ runTransactionI @IndexedChildDomain ChildA $ \_ ->
                        pure (id, [Added 1, Added 2])
            firstBatch <- readChan batches
            _ <-
                runEff
                    . runAggregate backend
                    . runSubAggregateI @IndexedChildDomain @IndexedParentDomain
                        routeChild
                        childValue
                        ChildChanged
                    $ runTransactionI @IndexedChildDomain ChildA $ \_ ->
                        pure (id, [Added 3])
            secondBatch <- readChan batches

            firstBatch
                `shouldBe` ( Indexed "a"
                           , [ChildChanged (Added 1), ChildChanged (Added 2)]
                           )
            secondBatch
                `shouldBe` (Indexed "a", [ChildChanged (Added 3)])

        it "persists nothing when the child callback fails and remains usable" $ do
            backend <-
                createForgetful
                    (\model event -> applyParentPayload model (storedEvent event))
                    (ParentModel 0 0)

            failed <- try @SomeException $
                runEff
                    . runAggregate backend
                    . runSubAggregateI @IndexedChildDomain @IndexedParentDomain
                        routeChild
                        childValue
                        ChildChanged
                    $ runTransactionI @IndexedChildDomain ChildA $ \_ ->
                        liftIO
                            ( ioError (userError "child callback failed")
                                :: IO (Int -> (), [ChildEvent])
                            )
            failed `shouldSatisfy` isLeft

            (parentHistory, parentModel) <-
                runEff
                    . runProjection backend
                    $ (,)
                        <$> getEventListI @IndexedParentDomain (Indexed "a")
                        <*> getModelI @IndexedParentDomain (Indexed "a")
            parentHistory `shouldBe` []
            parentModel `shouldBe` ParentModel 0 0

            result <-
                runEff
                    . runAggregate backend
                    . runSubAggregateI @IndexedChildDomain @IndexedParentDomain
                        routeChild
                        childValue
                        ChildChanged
                    $ runTransactionI @IndexedChildDomain ChildA $ \_ ->
                        pure (id, [Added 2])
            result `shouldBe` 2

        it "retains committed events when updated model projection fails after commit" $ do
            backend <-
                createForgetful
                    (\model event -> applyParentPayload model (storedEvent event))
                    (ParentModel 0 0)

            failed <- try @SomeException $
                ( runEff
                    . runAggregate backend
                    . runSubAggregateI @IndexedChildDomain @IndexedParentDomain
                        routeChild
                        ( \model ->
                            if childValue model == 0
                                then 0
                                else error "updated model projection failed"
                        )
                        ChildChanged
                )
                    ( runTransactionI @IndexedChildDomain ChildA $ \childModel ->
                        childModel `seq` pure (id, [Added 1])
                    )
                    >>= evaluate
            failed `shouldSatisfy` isLeft

            parentHistory <-
                runEff
                    . runProjection backend
                    $ getEventListI @IndexedParentDomain (Indexed "a")
            fmap storedEvent parentHistory `shouldBe` [ChildChanged (Added 1)]

    describe "mapping laws" $ do
        it "round-trips injected events and commutes with parent application" $ do
            let childEvents = [Added 2, ParentWideObserved 7, Added (-1)]
                parentEvents =
                    [ ChildChanged (Added 2)
                    , ParentWide 7
                    , ParentIgnored 4
                    ]
                initialParent = ParentModel 3 10

            fmap (projectChildEvent . ChildChanged) childEvents
                `shouldBe` fmap Just childEvents
            childValue
                (foldl' applyParentPayload initialParent (fmap ChildChanged childEvents))
                `shouldBe` foldl' applyChildPayload (childValue initialParent) childEvents
            mapM_
                ( \event ->
                    childValue (applyParentPayload initialParent event)
                        `shouldBe` case projectChildEvent event of
                            Nothing -> childValue initialParent
                            Just childEvent ->
                                applyChildPayload (childValue initialParent) childEvent
                )
                parentEvents

    describe "NoIndex specializations" $ do
        it "runSubAggregate agrees with runSubAggregateI" $ do
            wrapperBackend <-
                createForgetful
                    (\model event -> applyParentPayload model (storedEvent event))
                    (ParentModel 0 0)
            indexedBackend <-
                createForgetful
                    (\model event -> applyParentPayload model (storedEvent event))
                    (ParentModel 0 0)

            wrapperResult <-
                runEff
                    . runAggregate wrapperBackend
                    . runSubAggregate @NoIndexChildDomain @NoIndexParentDomain
                        childValue
                        ChildChanged
                    $ runTransaction @NoIndexChildDomain $ \_ ->
                        pure (id, [Added 2, Added 3])
            indexedResult <-
                runEff
                    . runAggregate indexedBackend
                    . runSubAggregateI @NoIndexChildDomain @NoIndexParentDomain
                        (const NoIndex)
                        childValue
                        ChildChanged
                    $ runTransaction @NoIndexChildDomain $ \_ ->
                        pure (id, [Added 2, Added 3])
            wrapperParent <-
                runEff
                    . runProjection wrapperBackend
                    $ (,)
                        <$> getModel @NoIndexParentDomain
                        <*> getEventList @NoIndexParentDomain
            indexedParent <-
                runEff
                    . runProjection indexedBackend
                    $ (,)
                        <$> getModel @NoIndexParentDomain
                        <*> getEventList @NoIndexParentDomain

            wrapperResult `shouldBe` indexedResult
            fst wrapperParent `shouldBe` fst indexedParent
            fmap storedEvent (snd wrapperParent)
                `shouldBe` fmap storedEvent (snd indexedParent)

        it "runSubProjection agrees with runSubProjectionI" $ do
            wrapperBackend <-
                createForgetful
                    (\model event -> applyParentPayload model (storedEvent event))
                    (ParentModel 0 0)
            indexedBackend <-
                createForgetful
                    (\model event -> applyParentPayload model (storedEvent event))
                    (ParentModel 0 0)
            runEff
                . runAggregate wrapperBackend
                $ runTransaction @NoIndexParentDomain $ \_ ->
                    pure
                        ( const ()
                        , [ ChildChanged (Added 2)
                          , ParentIgnored 9
                          , ParentWide 4
                          ]
                        )
            runEff
                . runAggregate indexedBackend
                $ runTransaction @NoIndexParentDomain $ \_ ->
                    pure
                        ( const ()
                        , [ ChildChanged (Added 2)
                          , ParentIgnored 9
                          , ParentWide 4
                          ]
                        )

            wrapperResult <-
                runEff
                    . runProjection wrapperBackend
                    . runSubProjection @NoIndexChildDomain @NoIndexParentDomain
                        childValue
                        projectChildEvent
                    $ (,)
                        <$> getModel @NoIndexChildDomain
                        <*> (fmap storedEvent <$> getEventList @NoIndexChildDomain)
            indexedResult <-
                runEff
                    . runProjection indexedBackend
                    . runSubProjectionI @NoIndexChildDomain @NoIndexParentDomain
                        (const NoIndex)
                        childValue
                        projectChildEvent
                    $ (,)
                        <$> getModel @NoIndexChildDomain
                        <*> (fmap storedEvent <$> getEventList @NoIndexChildDomain)
            wrapperResult `shouldBe` indexedResult

        it "runSubDomain agrees with runSubDomainI" $ do
            wrapperBackend <-
                createForgetful
                    (\model event -> applyParentPayload model (storedEvent event))
                    (ParentModel 0 0)
            indexedBackend <-
                createForgetful
                    (\model event -> applyParentPayload model (storedEvent event))
                    (ParentModel 0 0)

            wrapperResult <-
                runEff
                    . runProjection wrapperBackend
                    . runAggregate wrapperBackend
                    . runSubDomain @NoIndexChildDomain @NoIndexParentDomain
                        childValue
                        ChildChanged
                        projectChildEvent
                    $ do
                        result <- runTransaction @NoIndexChildDomain $ \_ ->
                            pure (id, [Added 2, Added 3])
                        history <- fmap storedEvent <$> getEventList @NoIndexChildDomain
                        pure (result, history)
            indexedResult <-
                runEff
                    . runProjection indexedBackend
                    . runAggregate indexedBackend
                    . runSubDomainI @NoIndexChildDomain @NoIndexParentDomain
                        (const NoIndex)
                        childValue
                        ChildChanged
                        projectChildEvent
                    $ do
                        result <- runTransaction @NoIndexChildDomain $ \_ ->
                            pure (id, [Added 2, Added 3])
                        history <- fmap storedEvent <$> getEventList @NoIndexChildDomain
                        pure (result, history)
            wrapperParent <-
                runEff
                    . runProjection wrapperBackend
                    $ getModel @NoIndexParentDomain
            indexedParent <-
                runEff
                    . runProjection indexedBackend
                    $ getModel @NoIndexParentDomain

            wrapperResult `shouldBe` indexedResult
            wrapperParent `shouldBe` indexedParent
