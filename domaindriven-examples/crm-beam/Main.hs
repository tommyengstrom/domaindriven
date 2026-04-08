{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Api (CustomersApi)
import CrmBeamProjection (CrmDb, createCrmBackend)
import DomainDriven (Aggregate, NoIndex, Projection, runAggregate, runProjection)
import DomainDriven.FieldNameAsPath (FieldNameAsPathApi, FieldNameAsPathServer (..))
import DomainDriven.Persistance.Postgres (PostgresBeam, simplePool)
import Effectful hiding ((:>))
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Event (CrmEvent)
import Model (CrmDomain, CrmModel)
import Network.Wai.Handler.Warp (run)
import Servant hiding (throwError)
import Servant qualified
import Server (customersServer)
import Database.PostgreSQL.Simple (connectPostgreSQL)
import Prelude

mkApp :: PostgresBeam NoIndex CrmDb CrmModel CrmEvent -> Application
mkApp backend =
    serve (Proxy @(FieldNameAsPathApi CustomersApi))
        $ hoistServer (Proxy @(FieldNameAsPathApi CustomersApi)) runEffects
        $ FieldNameAsPathServer customersServer
  where
    runEffects
        :: Eff
            '[ Projection CrmDomain
             , Aggregate CrmDomain
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

main :: IO ()
main = do
    let port = 7890
    putStrLn $ "Running Beam CRM example on port " <> show port
    putStrLn "  Endpoints:"
    putStrLn "    GET  /list_                                    — list customers"
    putStrLn "    POST /create                                   — create customer"
    putStrLn "    GET  /detail/{customerId}/get_                 — get customer"
    putStrLn "    POST /detail/{customerId}/changeName           — rename customer"
    putStrLn "    POST /detail/{customerId}/orders/create        — create order"
    putStrLn "    POST /detail/{customerId}/orders/detail/{orderId}/changeStatus"

    connectionPool <-
        simplePool $
            connectPostgreSQL
                "host=localhost port=5432 user=postgres dbname=domaindriven password=postgres"
    backend <- createCrmBackend connectionPool
    run port $ mkApp backend
