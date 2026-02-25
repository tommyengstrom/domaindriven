-- | Getting Started — basic counter with in-memory persistence.
--
-- Demonstrates:
--   * Defining a model, events, and applyEvent
--   * Using Servant Generic API with Effectful effects (Aggregate & Projection)
--   * ForgetfulInMemory backend (no database required)
--   * GET /events endpoint via getEventList
{-# LANGUAGE OverloadedRecordDot #-}
module Main where

import Control.Monad (when)
import Data.Aeson (FromJSON, ToJSON)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import DomainDriven
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
    { counter :: Int
    , previousCounter :: Int
    } deriving (Show, Generic)


--------------------------------------------------------------------------------
-- Define events
--------------------------------------------------------------------------------
data CounterEvent
    = CounterIncreased
    | CounterDecreased
    deriving (Show, Generic, ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Define event handler
--------------------------------------------------------------------------------
applyEvent :: CounterModel -> Stored CounterEvent -> CounterModel
applyEvent (CounterModel i _) (Stored ev _timestamp _uuid) = case ev of
    CounterIncreased -> CounterModel (i + 1) i
    CounterDecreased -> CounterModel (i - 1) i


--------------------------------------------------------------------------------
-- Use Servant to define the API
--------------------------------------------------------------------------------

-- | Envelope for returning stored events over JSON.
data StoredEvent = StoredEvent
    { event :: CounterEvent
    , timestamp :: UTCTime
    , uuid :: UUID
    }
    deriving (Show, Generic, ToJSON)

data CounterAPI mode = CounterAPI
    { get :: mode :- Get '[JSON] Int
    , increase :: mode :- "increase" :> Post '[JSON] Int
    , decrease :: mode :- "decrease" :> Post '[JSON] Int
    , events :: mode :- "events" :> Get '[JSON] [StoredEvent]
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
            pure (\a -> a.counter, [CounterIncreased])
        , decrease = runTransaction \m -> do
            when
                (m.counter <= 0)
                (throwError err422{errBody = "Counter cannot go below zero"})
            pure (\a -> a.counter, [CounterDecreased])
        , events = do
            storedEvents <- getEventList
            pure $ map toStoredEvent storedEvents
        }
  where
    toStoredEvent :: Stored CounterEvent -> StoredEvent
    toStoredEvent (Stored ev ts uid) = StoredEvent ev ts uid

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
