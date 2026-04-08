module Main where

import Control.Monad (when)
import Database.PostgreSQL.Simple (connectPostgreSQL)
import DomainDriven
import DomainDriven.Persistance.Postgres (PostgresBeam, simplePool)
import Effectful hiding ((:>))
import Effectful qualified
import Effectful.Error.Static
import Network.Wai.Handler.Warp (run)
import PostgresCounterBeam
import Servant hiding (throwError)
import Servant qualified
import Servant.API.Generic
import Servant.Server.Generic (AsServerT, genericServeT)
import Prelude

data CounterAPI mode = CounterAPI
    { get :: mode :- Get '[JSON] Int
    , increase :: mode :- "increase" :> ReqBody '[JSON] Int :> Post '[JSON] Int
    , decrease :: mode :- "decrease" :> ReqBody '[JSON] Int :> Post '[JSON] Int
    }
    deriving stock (Generic)

counterServer
    :: ( BeamProjection CounterDb Effectful.:> es
       , BeamAggregate CounterDomain CounterDb Effectful.:> es
       , Error ServerError Effectful.:> es
       )
    => CounterAPI (AsServerT (Eff es))
counterServer =
    CounterAPI
        { get = runPg $ loadCounterQuery counterDbSettings
        , increase = \amount ->
            runBeamTransaction @CounterDomain @CounterDb $
                pure (loadCounterQuery counterDbSettings, [CounterIncreasedBy amount])
        , decrease = \amount -> do
            current <- runPg $ loadCounterQuery counterDbSettings
            when
                (current - amount < 0)
                (throwError err422{errBody = "Counter cannot go below zero"})
            runBeamTransaction @CounterDomain @CounterDb $
                pure (loadCounterQuery counterDbSettings, [CounterDecreasedBy amount])
        }

mkCounterServer
    :: PostgresBeam NoIndex CounterDb Int CounterEvent
    -> Application
mkCounterServer backend =
    genericServeT runEffects counterServer
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
    let port = 7879
    putStrLn $ "Running Beam-backed Postgres counter on port " <> show port

    connectionPool <-
        simplePool $
            connectPostgreSQL
                "host=localhost port=5432 user=postgres dbname=domaindriven password=postgres"
    backend <- createBackend connectionPool

    run port $ mkCounterServer backend
