{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Data.Aeson
import DomainDriven.Persistance.Class (Stored(..), NoIndex(..))
import DomainDriven.Persistance.ForgetfulInMemory (createForgetful, ForgetfulInMemory)
import DomainDriven.Effectful
import DomainDriven.Effectful.Interpreter.InMemory
import Effectful
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.API.Generic
import Servant.Server.Generic (genericServeT, AsServerT)
import Prelude

--------------------------------------------------------------------------------
-- 1. Define the model
--------------------------------------------------------------------------------
newtype CounterModel = CounterModel {getCounter :: Int}
    deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
-- 2. Define events and how to apply them
--------------------------------------------------------------------------------
data CounterEvent
    = Increase
    | Decrease
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

applyEvent :: CounterModel -> Stored CounterEvent -> CounterModel
applyEvent (CounterModel i) (Stored ev _ _) = CounterModel $ case ev of
    Increase -> i + 1
    Decrease -> i - 1

-- Define the EventSourced instance to use helpers
instance EventSourced CounterModel CounterEvent NoIndex

--------------------------------------------------------------------------------
-- 3. Define the API using record-based approach with standard Servant combinators
--------------------------------------------------------------------------------
data CounterAPI mode = CounterAPI
    { current  :: mode Servant.:- Get '[JSON] Int
    , increase :: mode Servant.:- Post '[JSON] Int
    , decrease :: mode Servant.:- Post '[JSON] Int
    , safeDecrease :: mode Servant.:- "safe-decrease" Servant.:> Post '[JSON] (Maybe Int)
    } deriving (Generic)

--------------------------------------------------------------------------------
-- 4. Implement the server handlers using simplified helpers
--------------------------------------------------------------------------------

-- | Counter handlers using simplified helper functions
counterHandlers
    :: ( Projection CounterModel CounterEvent NoIndex Effectful.:> es
       , Aggregate CounterModel CounterEvent NoIndex Effectful.:> es
       )
    => CounterAPI (AsServerT (Eff es))
counterHandlers = CounterAPI
    { current = getCounter <$> queryModel @CounterModel @CounterEvent
    , increase = simpleCommand @CounterModel @CounterEvent getCounter [Increase]
    , decrease = simpleCommand @CounterModel @CounterEvent getCounter [Decrease]
    , safeDecrease = conditionalCommand @CounterModel @CounterEvent 
        (\(CounterModel n) -> if n > 0 then Just [Decrease] else Nothing)
        getCounter
    }

--------------------------------------------------------------------------------
-- 5. Wire up the server with effect interpreters
--------------------------------------------------------------------------------

mkCounterServer :: ForgetfulInMemory CounterModel NoIndex CounterEvent -> Application
mkCounterServer backend = 
    genericServeT runEffects counterHandlers
  where
    runEffects :: Eff '[Projection CounterModel CounterEvent NoIndex, 
                       Aggregate CounterModel CounterEvent NoIndex, 
                       IOE] a -> Handler a
    runEffects = liftIO . runEff . runAggregateInMemory backend . runProjectionInMemory backend NoIndex

main :: IO ()
main = do
    let port = 7878
    putStrLn $ "Running Effectful counter with helpers on port " <> show port
    
    -- Initialize the in-memory backend
    backend <- createForgetful @NoIndex applyEvent (CounterModel 0)
    
    -- Create and run the application
    let app = mkCounterServer backend
    
    run port app