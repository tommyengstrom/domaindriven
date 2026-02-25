{-# LANGUAGE OverloadedRecordDot #-}
module Main where

import Control.Monad (when)
import DomainDriven.Effectful
import DomainDriven.Persistance.ForgetfulInMemory (ForgetfulInMemory, createForgetful)
import Effectful hiding ((:>))
import Effectful qualified
import Effectful.Error.Static
import Network.Wai.Handler.Warp (run)
import Servant hiding (throwError)
import Servant qualified
import Servant.API.Generic
import Servant.Server.Generic (AsServerT, genericServeT)
import Prelude

--------------------------------------------------------------------------------
-- Define the model
--------------------------------------------------------------------------------
data CounterModel = CounterModel
    { counter ::  Int
    , previousCounter :: Int
    } deriving (Show, Generic)


--------------------------------------------------------------------------------
-- Define events
--------------------------------------------------------------------------------
data CounterEvent
    = CounterIncreased
    | CounterDecreased
    deriving (Show)

--------------------------------------------------------------------------------
-- Define event handler
--------------------------------------------------------------------------------
applyEvent :: CounterModel -> Stored CounterEvent -> CounterModel
applyEvent (CounterModel i _) (Stored ev _timestamp _uuid) = case ev of
    CounterIncreased -> CounterModel (i + 1) i
    CounterDecreased -> CounterModel (i - 1) i


--------------------------------------------------------------------------------
-- Use Servant to define the Commands
--------------------------------------------------------------------------------
data CounterAPI mode = CounterAPI
    { get :: mode :- Get '[JSON] Int
    , increase :: mode :- "increase" :> Post '[JSON] Int
    , decrease :: mode :- "decrease" :> Post '[JSON] Int
    }
    deriving (Generic)

--------------------------------------------------------------------------------
-- Implement the server handlers using Effectful effects
--------------------------------------------------------------------------------

type CounterDomain = Domain CounterModel CounterEvent NoIndex

-- | Counter handlers using Effectful effects
counterServer
    :: ( Projection CounterDomain Effectful.:> es
       , Aggregate CounterDomain Effectful.:> es
       , Error ServerError Effectful.:> es
       )
    => CounterAPI (AsServerT (Eff es))
counterServer =
    CounterAPI
        { get = do
            CounterModel {counter} <- getModel
            pure counter
        , increase = runTransaction \_ -> do
            pure (\a -> a.counter , [CounterIncreased])
        , decrease = runTransaction \m -> do
            when
                (m.counter <= 0)
                (throwError err422{errBody = "Counter cannot go below zero"})
            pure (\a -> a.counter, [CounterDecreased])
        }

--------------------------------------------------------------------------------
-- Create the servant application.
-- Here we have to run all the effects and transform it to Servant's Handler monad.
--------------------------------------------------------------------------------
mkCounterServer
    :: ForgetfulInMemory CounterModel NoIndex CounterEvent
    -> Application
mkCounterServer backend =
    genericServeT runEffects counterServer
  where
    runEffects
        :: Eff
            '[ Projection CounterDomain
             , Aggregate CounterDomain
             , Error ServerError
             , IOE
             ]
            a
        -> Handler a
    runEffects m = do
        a <-
            liftIO
                . runEff
                . runErrorNoCallStack @ServerError
                . runAggregate backend
                $ runProjection backend m
        either Servant.throwError pure a

--------------------------------------------------------------------------------
-- Run the server
--------------------------------------------------------------------------------
main :: IO ()
main = do
    let port = 7878
    putStrLn $ "Running Effectful counter on port " <> show port

    -- Initialize the in-memory backend
    backend <- createForgetful applyEvent (CounterModel 0 0)

    -- Create and run the application
    run port $ mkCounterServer backend
