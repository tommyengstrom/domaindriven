{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Test.Hspec
import DomainDriven.Effectful
import DomainDriven.Effectful.Interpreter.InMemory
import DomainDriven.Persistance.Class (Stored(..), NoIndex(..))
import DomainDriven.Persistance.ForgetfulInMemory (createForgetful)
import Effectful
import Data.Aeson
import GHC.Generics (Generic)
import Prelude

--------------------------------------------------------------------------------
-- Test model and events
--------------------------------------------------------------------------------

newtype TestModel = TestModel { getValue :: Int }
    deriving (Show, Eq, Generic)

data TestEvent = Add Int | Reset
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

applyTestEvent :: TestModel -> Stored TestEvent -> TestModel
applyTestEvent (TestModel n) (Stored ev _ _) = case ev of
    Add x -> TestModel (n + x)
    Reset -> TestModel 0

--------------------------------------------------------------------------------
-- Test effects
--------------------------------------------------------------------------------

getValue' :: Projection TestModel TestEvent NoIndex :> es => Eff es Int
getValue' = getValue <$> getModel @TestModel @TestEvent @NoIndex

addValue :: Aggregate TestModel TestEvent NoIndex :> es => Int -> Eff es Int
addValue x = runTransaction @TestModel @TestEvent @NoIndex NoIndex $ \_ ->
    pure (getValue, [Add x])

resetValue :: Aggregate TestModel TestEvent NoIndex :> es => Eff es Int
resetValue = runTransaction @TestModel @TestEvent @NoIndex NoIndex $ \_ ->
    pure (getValue, [Reset])

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
    describe "Projection effect interpreter" $ do
        it "should read initial model state" $ do
            backend <- createForgetful @NoIndex applyTestEvent (TestModel 42)
            result <- runEff 
                . runProjectionInMemory backend NoIndex 
                $ getValue'
            result `shouldBe` 42
        
        it "should read empty event list initially" $ do
            backend <- createForgetful @NoIndex applyTestEvent (TestModel 0)
            events <- runEff 
                . runProjectionInMemory backend NoIndex 
                $ getEventList @TestModel @TestEvent @NoIndex
            events `shouldBe` []
    
    describe "Aggregate effect interpreter" $ do
        it "should handle commands and update state" $ do
            backend <- createForgetful @NoIndex applyTestEvent (TestModel 0)
            
            -- Add 10
            result1 <- runEff 
                . runAggregateInMemory backend 
                $ addValue 10
            result1 `shouldBe` 10
            
            -- Verify state was updated
            currentValue <- runEff 
                . runProjectionInMemory backend NoIndex 
                $ getValue'
            currentValue `shouldBe` 10
            
            -- Add another 5
            result2 <- runEff 
                . runAggregateInMemory backend 
                $ addValue 5
            result2 `shouldBe` 15
            
            -- Verify final state
            finalValue <- runEff 
                . runProjectionInMemory backend NoIndex 
                $ getValue'
            finalValue `shouldBe` 15
        
        it "should store events" $ do
            backend <- createForgetful @NoIndex applyTestEvent (TestModel 0)
            
            -- Perform some operations
            _ <- runEff . runAggregateInMemory backend $ addValue 10
            _ <- runEff . runAggregateInMemory backend $ addValue 5
            _ <- runEff . runAggregateInMemory backend $ resetValue
            
            -- Check events were stored
            events <- runEff 
                . runProjectionInMemory backend NoIndex 
                $ getEventList @TestModel @TestEvent @NoIndex
            
            length events `shouldBe` 3
            map storedEvent events `shouldBe` [Add 10, Add 5, Reset]
        
        it "should handle multiple aggregates with indices" $ do
            backend <- createForgetful @Int applyTestEvent (TestModel 0)
            
            -- Work with aggregate 1
            _ <- runEff . runAggregateInMemory backend $ 
                runTransaction @TestModel @TestEvent @Int 1 $ \_ ->
                    pure (getValue, [Add 10])
            
            -- Work with aggregate 2
            _ <- runEff . runAggregateInMemory backend $ 
                runTransaction @TestModel @TestEvent @Int 2 $ \_ ->
                    pure (getValue, [Add 20])
            
            -- Check both aggregates have different states
            value1 <- runEff . runProjectionInMemory backend 1 $ 
                getModel @TestModel @TestEvent @Int
            value2 <- runEff . runProjectionInMemory backend 2 $ 
                getModel @TestModel @TestEvent @Int
            
            getValue value1 `shouldBe` 10
            getValue value2 `shouldBe` 20