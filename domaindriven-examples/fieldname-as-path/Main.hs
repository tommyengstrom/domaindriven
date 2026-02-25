-- | FieldNameAsPath — record field names become URL path segments automatically.
--
-- Demonstrates:
--   * FieldNameAsPathApi: wraps a Servant Generic record so field names become paths
--   * FieldNameAsPathServer: serves the wrapped API
--   * Same counter domain as simple/, different routing approach
--
-- Endpoints:  GET /get,  POST /increase,  POST /decrease
-- (no "increase" :> or "decrease" :> needed in the API definition)
{-# LANGUAGE OverloadedRecordDot #-}
module Main where

import Control.Monad (when)
import Data.Aeson (FromJSON, ToJSON)
import DomainDriven
import DomainDriven.FieldNameAsPath
import DomainDriven.Persistance.ForgetfulInMemory (ForgetfulInMemory, createForgetful)
import Effectful hiding ((:>))
import Effectful qualified
import Effectful.Error.Static
import Network.Wai.Handler.Warp (run)
import Servant hiding (throwError)
import Servant qualified
import Servant.API.Generic
import Servant.Server.Generic (AsServerT)
import Prelude

--------------------------------------------------------------------------------
-- Define the model
--------------------------------------------------------------------------------
data CounterModel = CounterModel
    { counter :: Int
    , previousCounter :: Int
    }
    deriving (Show, Generic)

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
-- Define the API — field names become paths automatically
--------------------------------------------------------------------------------

-- | No path strings needed: /get, /increase, /decrease are derived from field names.
data CounterAPI mode = CounterAPI
    { get :: mode :- Get '[JSON] Int
    , increase :: mode :- Post '[JSON] Int
    , decrease :: mode :- Post '[JSON] Int
    }
    deriving (Generic)

-- | Use default apiTagFromLabel (= id), so field name is used as-is.
instance ApiTagFromLabel CounterAPI

--------------------------------------------------------------------------------
-- Implement the server handlers
--------------------------------------------------------------------------------

type CounterDomain = Domain CounterModel CounterEvent NoIndex

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
        }

--------------------------------------------------------------------------------
-- Create the servant application using FieldNameAsPath
--------------------------------------------------------------------------------

-- Note: we use serve/hoistServer instead of genericServeT because
-- FieldNameAsPathApi provides its own HasServer instance that derives
-- paths from field names.
mkCounterServer
    :: ForgetfulInMemory CounterModel NoIndex CounterEvent
    -> Application
mkCounterServer backend =
    serve (Proxy @(FieldNameAsPathApi CounterAPI))
        $ hoistServer (Proxy @(FieldNameAsPathApi CounterAPI)) runEffects
        $ FieldNameAsPathServer counterServer
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
    let port = 7880
    putStrLn $ "Running FieldNameAsPath counter on port " <> show port
    putStrLn "  Endpoints: GET /get, POST /increase, POST /decrease"

    -- Initialize the in-memory backend
    backend <- createForgetful applyEvent (CounterModel 0 0)

    -- Create and run the application
    run port $ mkCounterServer backend
