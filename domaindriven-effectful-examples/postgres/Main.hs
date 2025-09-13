module Main where

import Control.Monad (when)
import Data.Aeson
import DomainDriven.Effectful
import DomainDriven.Effectful.Interpreter.Postgres
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
type CounterModel = Int

--------------------------------------------------------------------------------
-- Define events
--------------------------------------------------------------------------------
data CounterEvent
    = Increase
    | Decrease
    deriving (Show, Generic, ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Define event handler
--------------------------------------------------------------------------------
applyEvent :: CounterModel -> Stored CounterEvent -> CounterModel
applyEvent i (Stored ev _timestamp _uuid) = case ev of
    Increase -> i + 1
    Decrease -> i - 1

-- Define the domain, used to cary the type constraints
type CounterDomain = Domain CounterModel CounterEvent NoIndex

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

-- | Counter handlers using Effectful effects
counterServer
    :: ( Projection CounterDomain Effectful.:> es
       , Aggregate CounterDomain Effectful.:> es
       , Error ServerError Effectful.:> es
       )
    => CounterAPI (AsServerT (Eff es))
counterServer =
    CounterAPI
        { get = getModel
        , increase = runTransaction do
            pure (id, [Increase])
        , decrease = runTransaction do
            m <- getModel
            when
                (m <= 0)
                (throwError err422{errBody = "Counter cannot go below zero"})
            pure (id, [Decrease])
        }

--------------------------------------------------------------------------------
-- Create the servant application.
-- Here we have to run all the effects and transform it to Servant's Handler monad.
--------------------------------------------------------------------------------
mkCounterServer :: PostgresEvent NoIndex CounterModel CounterEvent
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
                . runAggregatePostgres backend
                $ runProjectionPostgres backend m
        either Servant.throwError pure a

eventTable :: EventTable
eventTable = InitialVersion "counter_events"
--------------------------------------------------------------------------------
-- Run the server
--------------------------------------------------------------------------------
main :: IO ()
main = do
    let port = 7878
    putStrLn $ "Running Effectful counter on port " <> show port

    -- Initialize the in-memory backend
    connectionPool <- simplePool undefined
    backend <- postgresWriteModel
        connectionPool
        eventTable
        applyEvent
        (0 :: CounterModel)
    -- Create and run the application
    run port $ mkCounterServer backend
