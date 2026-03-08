{-# LANGUAGE OverloadedRecordDot #-}

-- | CRM Example — Customers > Orders > OrderItems (3-level hierarchy).
--
-- Demonstrates all shared patterns identified across PCP and FluentQuest:
--
--   * Entity handler pattern (withCustomer, withOrder)
--   * Nested FieldNameAsPath API with Captures at each level
--   * Hierarchical applyEvent dispatch with optics (3 levels)
--   * Event wrapping helpers (wrapCustE, wrapOrdE)
--   * Dual lookup helpers (Eff + Pure variants)
--   * Effects type alias with qualified Effectful.:>
--   * Command types (request body newtypes)
--   * Effect stack wiring with ForgetfulInMemory backend
--
-- Run with: cabal run crm-example
-- Then:
--   curl -X POST localhost:7890/list_               # list customers
--   curl -X POST localhost:7890/create -d '{"name":"Acme","email":"a@b.com"}'
module Main where

import Api (CustomersApi)
import DomainDriven (Aggregate, NoIndex, Projection, runAggregate, runProjection)
import DomainDriven.FieldNameAsPath (FieldNameAsPathApi, FieldNameAsPathServer (..))
import DomainDriven.Persistance.ForgetfulInMemory (ForgetfulInMemory, createForgetful)
import Effectful hiding ((:>))
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Event (CrmEvent)
import EventHandler (applyEvent)
import Model (CrmDomain, CrmModel, emptyCrmModel)
import Network.Wai.Handler.Warp (run)
import Servant hiding (throwError)
import Servant qualified
import Server (customersServer)
import Prelude

--------------------------------------------------------------------------------
-- Application wiring
--------------------------------------------------------------------------------

mkApp :: ForgetfulInMemory CrmModel NoIndex CrmEvent -> Application
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

--------------------------------------------------------------------------------
-- Entry point
--------------------------------------------------------------------------------

main :: IO ()
main = do
    let port = 7890
    putStrLn $ "Running CRM example on port " <> show port
    putStrLn "  Endpoints:"
    putStrLn "    GET  /list_                                    — list customers"
    putStrLn "    POST /create                                   — create customer"
    putStrLn "    GET  /detail/{customerId}/get_                 — get customer"
    putStrLn "    POST /detail/{customerId}/changeName           — rename customer"
    putStrLn "    POST /detail/{customerId}/orders/create        — create order"
    putStrLn "    POST /detail/{customerId}/orders/detail/{orderId}/changeStatus"
    backend <- createForgetful applyEvent emptyCrmModel
    run port $ mkApp backend
