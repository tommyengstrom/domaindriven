module DomainDriven.Effectful.InMemorySpec (spec) where

import DomainDriven.Effectful.Aggregate
import DomainDriven.Effectful.Domain
import DomainDriven.Effectful.Interpreter.InMemory
import DomainDriven.Effectful.Projection
import DomainDriven.Persistance.Class (Indexed (..), NoIndex (..), Stored (..))
import DomainDriven.Persistance.ForgetfulInMemory ()
import Effectful
import Test.Hspec
import Prelude

type TestModel = Int

data TestEvent = AddOne | SubtractOne | Reset
    deriving (Show, Eq)

applyTestEvent :: TestModel -> Stored TestEvent -> TestModel
applyTestEvent m ev = case storedEvent ev of
    AddOne -> m + 1
    SubtractOne -> m - 1
    Reset -> 0

type TestDomain = Domain TestModel TestEvent NoIndex
type IndexedTestDomain = Domain TestModel TestEvent Indexed

runTest
    :: ForgetfulInMemory TestModel NoIndex TestEvent
    -> Eff '[Aggregate TestDomain, Projection TestDomain, IOE] a
    -> IO a
runTest backend =
    runEff
        . runProjectionInMemory backend
        . runAggregateInMemory backend

runIndexedTest
    :: ForgetfulInMemory TestModel Indexed TestEvent
    -> Eff '[Aggregate IndexedTestDomain, Projection IndexedTestDomain, IOE] a
    -> IO a
runIndexedTest backend =
    runEff
        . runProjectionInMemory backend
        . runAggregateInMemory backend

spec :: Spec
spec = do
    describe "Aggregate interpreter" $ do
        it "runTransaction persists state via getModel" $ do
            backend <- createForgetful applyTestEvent (0 :: TestModel)
            runTest backend $
                runTransaction @TestDomain $ \_ -> pure (const (), [AddOne])
            m <- runTest backend (getModel @TestDomain)
            m `shouldBe` 1

        it "runTransaction applies returnFun to the updated model" $ do
            backend <- createForgetful applyTestEvent (0 :: TestModel)
            result <- runTest backend $
                runTransaction @TestDomain $ \_ ->
                    pure (id, [AddOne, AddOne])
            result `shouldBe` 2

        it "callback can use effects from the stack via localSeqUnlift" $ do
            backend <- createForgetful applyTestEvent (0 :: TestModel)
            result <- runTest backend $ do
                runTransaction @TestDomain $ \_ -> pure (const (), [AddOne])
                runTransaction @TestDomain $ \_ -> do
                    m <- getModel @TestDomain
                    pure (const m, [])
            result `shouldBe` 1

    describe "Projection interpreter" $ do
        it "getEventList returns stored events in order" $ do
            backend <- createForgetful applyTestEvent (0 :: TestModel)
            runTest backend $
                runTransaction @TestDomain $ \_ -> pure (const (), [AddOne, SubtractOne])
            evs <- runTest backend (getEventList @TestDomain)
            map storedEvent evs `shouldBe` [AddOne, SubtractOne]

    describe "Indexed dispatch" $ do
        it "runTransactionI and getModelI dispatch to separate indices" $ do
            backend <- createForgetful applyTestEvent (0 :: TestModel)
            runIndexedTest backend $ do
                runTransactionI @IndexedTestDomain (Indexed "a") $ \_ ->
                    pure (const (), [AddOne])
                runTransactionI @IndexedTestDomain (Indexed "b") $ \_ ->
                    pure (const (), [AddOne, AddOne])
            ma <- runIndexedTest backend (getModelI @IndexedTestDomain (Indexed "a"))
            mb <- runIndexedTest backend (getModelI @IndexedTestDomain (Indexed "b"))
            ma `shouldBe` 1
            mb `shouldBe` 2
