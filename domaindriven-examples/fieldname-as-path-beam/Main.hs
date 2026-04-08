{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Control.Monad (when)
import Database.PostgreSQL.Simple (connectPostgreSQL)
import DomainDriven
import DomainDriven.FieldNameAsPath
import DomainDriven.Persistance.Postgres (PostgresBeam, simplePool)
import Effectful hiding ((:>))
import Effectful qualified
import Effectful.Error.Static
import Network.Wai.Handler.Warp (run)
import Servant hiding (throwError)
import Servant qualified
import Servant.API.Generic
import Servant.Server.Generic (AsServerT)
import SimpleCounterBeam
import Prelude

data CounterAPI mode = CounterAPI
    { get :: mode :- Get '[JSON] Int
    , increase :: mode :- Post '[JSON] Int
    , decrease :: mode :- Post '[JSON] Int
    }
    deriving stock (Generic)

instance ApiTagFromLabel CounterAPI

counterServer
    :: ( BeamProjection CounterDb Effectful.:> es
       , BeamAggregate CounterDomain CounterDb Effectful.:> es
       , Error ServerError Effectful.:> es
       )
    => CounterAPI (AsServerT (Eff es))
counterServer =
    CounterAPI
        { get = do
            CounterModel{counter} <- runPg $ loadCounterModelQuery projectionDb
            pure counter
        , increase =
            runBeamTransaction @CounterDomain @CounterDb $
                pure (loadCounterValueQuery projectionDb, [CounterIncreased])
        , decrease = do
            CounterModel{counter} <- runPg $ loadCounterModelQuery projectionDb
            when
                (counter <= 0)
                (throwError err422{errBody = "Counter cannot go below zero"})
            runBeamTransaction @CounterDomain @CounterDb $
                pure (loadCounterValueQuery projectionDb, [CounterDecreased])
        }
  where
    projectionDb = counterDbSettings "fieldname_counter_projection"

mkCounterServer
    :: PostgresBeam NoIndex CounterDb CounterModel CounterEvent
    -> Application
mkCounterServer backend =
    serve (Proxy @(FieldNameAsPathApi CounterAPI))
        $ hoistServer (Proxy @(FieldNameAsPathApi CounterAPI)) runEffects
        $ FieldNameAsPathServer counterServer
  where
    runEffects
        :: Eff
            '[ BeamProjection CounterDb
             , BeamAggregate CounterDomain CounterDb
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
                . runBeamAggregate backend
                $ runBeamProjection backend m
        either Servant.throwError pure a

main :: IO ()
main = do
    let port = 7880
    putStrLn $ "Running Beam FieldNameAsPath counter on port " <> show port
    putStrLn "  Endpoints: GET /get, POST /increase, POST /decrease"

    connectionPool <-
        simplePool $
            connectPostgreSQL
                "host=localhost port=5432 user=postgres dbname=domaindriven password=postgres"
    backend <-
        createCounterBackend
            "fieldname_counter_events"
            "fieldname_counter_projection"
            connectionPool

    run port $ mkCounterServer backend
