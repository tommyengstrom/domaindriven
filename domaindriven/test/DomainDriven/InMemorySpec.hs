module DomainDriven.InMemorySpec (spec) where

import DomainDriven.Aggregate
import DomainDriven.Domain
import DomainDriven.Interpreter
import DomainDriven.Projection
import DomainDriven.Persistance.Class (Indexed (..), NoIndex (..), Stored (..))
import DomainDriven.Persistance.ForgetfulInMemory
import Effectful
import Test.Hspec
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
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
        . runProjection backend
        . runAggregate backend

runTestWith
    :: ForgetfulInMemory TestModel NoIndex TestEvent
    -> (NoIndex -> TestModel -> [Stored TestEvent] -> IO ())
    -> Eff '[Aggregate TestDomain, Projection TestDomain, IOE] a
    -> IO a
runTestWith backend hook =
    runEff
        . runProjection backend
        . runAggregate (backend { updateHook = hook })

runIndexedTest
    :: ForgetfulInMemory TestModel Indexed TestEvent
    -> Eff '[Aggregate IndexedTestDomain, Projection IndexedTestDomain, IOE] a
    -> IO a
runIndexedTest backend =
    runEff
        . runProjection backend
        . runAggregate backend

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

    describe "PostUpdateHook" $ do
        it "hook receives correct model and events" $ do
            mvar <- newEmptyMVar
            let hook _ model events = putMVar mvar (model, map storedEvent events)
            backend <- createForgetful applyTestEvent (0 :: TestModel)
            runTestWith backend hook $
                runTransaction @TestDomain $ \_ -> pure (const (), [AddOne, AddOne])
            result <- takeMVar mvar
            result `shouldBe` (2, [AddOne, AddOne])

        it "hook fires for each transaction" $ do
            chan <- newChan
            let hook _ _ _ = writeChan chan ()
            backend <- createForgetful applyTestEvent (0 :: TestModel)
            runTestWith backend hook $ do
                runTransaction @TestDomain $ \_ -> pure (const (), [AddOne])
                runTransaction @TestDomain $ \_ -> pure (const (), [AddOne])
            readChan chan
            readChan chan

        it "hook fires once per transaction" $ do
            mvar <- newEmptyMVar
            let hook _ _ _ = putMVar mvar ()
            backend <- createForgetful applyTestEvent (0 :: TestModel)
            runTestWith backend hook $
                runTransaction @TestDomain $ \_ -> pure (const (), [AddOne])
            takeMVar mvar

        it "failing hook does not crash the command" $ do
            mvar <- newEmptyMVar
            let hook _ _ _ = putMVar mvar () >> error "hook explosion"
            backend <- createForgetful applyTestEvent (0 :: TestModel)
            result <- runTestWith backend hook $
                runTransaction @TestDomain $ \_ -> pure (id, [AddOne])
            takeMVar mvar  -- wait for hook to fire
            result `shouldBe` 1

        it "no-op hook does not affect behavior" $ do
            backend <- createForgetful applyTestEvent (0 :: TestModel)
            runTestWith backend (\_ _ _ -> pure ()) $
                runTransaction @TestDomain $ \_ -> pure (const (), [AddOne])
            m <- runTestWith backend (\_ _ _ -> pure ()) (getModel @TestDomain)
            m `shouldBe` 1
