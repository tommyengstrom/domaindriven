-- | PostgreSQL persistence with event migration.
--
-- Demonstrates:
--   * PostgresEvent backend with connection pooling (simplePool)
--   * Event versioning: V1 (unit events) → V2 (events with Int payload)
--   * Wiring an EventTable with MigrateUsing into postgresWriteModel
--   * ReqBody for parameterised commands
module Main where

import Control.Monad (when)
import Database.PostgreSQL.Simple (connectPostgreSQL)
import DomainDriven.Effectful
import DomainDriven.Persistance.Postgres (PostgresEvent, postgresWriteModel, simplePool)
import Effectful hiding ((:>))
import Effectful qualified
import Effectful.Error.Static
import Event.V2
import EventMigration (eventTable)
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
-- Define event handler (using V2 events)
--------------------------------------------------------------------------------
applyEvent :: CounterModel -> Stored CounterEvent -> CounterModel
applyEvent i (Stored ev _timestamp _uuid) = case ev of
    CounterIncreasedBy n -> i + n
    CounterDecreasedBy n -> i - n

-- | Domain type carrying model, event, and index constraints.
type CounterDomain = Domain CounterModel CounterEvent NoIndex

--------------------------------------------------------------------------------
-- Use Servant to define the API
--------------------------------------------------------------------------------
data CounterAPI mode = CounterAPI
    { get :: mode :- Get '[JSON] Int
    , increase :: mode :- "increase" :> ReqBody '[JSON] Int :> Post '[JSON] Int
    , decrease :: mode :- "decrease" :> ReqBody '[JSON] Int :> Post '[JSON] Int
    }
    deriving (Generic)

--------------------------------------------------------------------------------
-- Implement the server handlers using Effectful effects
--------------------------------------------------------------------------------
counterServer
    :: ( Projection CounterDomain Effectful.:> es
       , Aggregate CounterDomain Effectful.:> es
       , Error ServerError Effectful.:> es
       )
    => CounterAPI (AsServerT (Eff es))
counterServer =
    CounterAPI
        { get = getModel
        , increase = \amount -> runTransaction \_ -> do
            pure (id, [CounterIncreasedBy amount])
        , decrease = \amount -> runTransaction \m -> do
            when
                (m - amount < 0)
                (throwError err422{errBody = "Counter cannot go below zero"})
            pure (id, [CounterDecreasedBy amount])
        }

--------------------------------------------------------------------------------
-- Create the servant application
--------------------------------------------------------------------------------
mkCounterServer
    :: PostgresEvent NoIndex CounterModel CounterEvent
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
    let port = 7879
    putStrLn $ "Running Effectful counter (Postgres) on port " <> show port

    -- Initialize the PostgreSQL backend with event migration
    connectionPool <-
        simplePool $
            connectPostgreSQL
                "host=localhost port=5432 user=postgres dbname=domaindriven password=postgres"
    backend <-
        postgresWriteModel
            connectionPool
            eventTable
            applyEvent
            (0 :: CounterModel)

    -- Create and run the application
    run port $ mkCounterServer backend
