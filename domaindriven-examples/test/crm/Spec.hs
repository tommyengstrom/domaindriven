{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Api qualified
import Command
import Data.Map.Strict qualified as Map
import Data.UUID qualified as UUID
import DomainDriven
import DomainDriven.Persistance.ForgetfulInMemory (ForgetfulInMemory, createForgetful)
import Effectful
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Event (CrmEvent)
import EventHandler (applyEvent)
import Model (CrmDomain, CrmModel (..), emptyCrmModel)
import Servant (NoContent (..), ServerError (errHTTPCode))
import Server
import Test.Hspec
import Types
import Prelude

runCrm
    :: ForgetfulInMemory CrmModel NoIndex CrmEvent
    -> Eff '[Projection CrmDomain, Aggregate CrmDomain, Error ServerError, GenId, IOE] a
    -> IO (Either ServerError a)
runCrm backend =
    runEff
        . runGenIdWith (pure UUID.nil)
        . runErrorNoCallStack @ServerError
        . runAggregate backend
        . runProjection backend

fixtureCustomerId :: CustomerId
fixtureCustomerId = CustomerId UUID.nil

fixtureOrderId :: OrderId
fixtureOrderId = OrderId UUID.nil

customerModel :: CrmModel
customerModel =
    CrmModel (Map.singleton fixtureCustomerId (Customer fixtureCustomerId "Alice" "alice@example.com" mempty))

main :: IO ()
main = hspec $ describe "CRM transaction results" $ do
    it "returns created and updated customers and orders" $ do
        backend <- createForgetful applyEvent emptyCrmModel
        result <- runCrm backend $ do
            createdCustomer <- customersServer.create (CreateCustomer "Alice" "alice@example.com")
            updatedCustomer <- (customerServer fixtureCustomerId).changeName (ChangeCustomerName "Alicia")
            createdOrder <- (ordersServer fixtureCustomerId).create (CreateOrder "First order")
            updatedOrder <- (orderServer fixtureCustomerId fixtureOrderId).changeStatus (ChangeOrderStatus Confirmed)
            pure (createdCustomer.name, updatedCustomer.name, createdOrder.description, updatedOrder.status)
        result `shouldBe` Right ("Alice", "Alicia", "First order", Confirmed)

    it "returns 404 for a missing customer without writing events" $ do
        backend <- createForgetful applyEvent emptyCrmModel
        result <- runCrm backend $ (ordersServer fixtureCustomerId).create (CreateOrder "Missing parent")
        either errHTTPCode (const 200) result `shouldBe` 404
        history <- runCrm backend $ getEventList @CrmDomain
        fmap length history `shouldBe` Right 0

    it "returns 404 for a missing order without writing events" $ do
        backend <- createForgetful applyEvent customerModel
        result <- runCrm backend $ (orderServer fixtureCustomerId fixtureOrderId).remove
        either errHTTPCode (const 200) result `shouldBe` 404
        history <- runCrm backend $ getEventList @CrmDomain
        fmap length history `shouldBe` Right 0

    it "returns NoContent when deleting an order and its customer" $ do
        backend <- createForgetful applyEvent customerModel
        result <- runCrm backend $ do
            _ <- (ordersServer fixtureCustomerId).create (CreateOrder "To delete")
            orderDeleted <- (orderServer fixtureCustomerId fixtureOrderId).remove
            afterOrder <- getModel @CrmDomain
            customerDeleted <- (customerServer fixtureCustomerId).remove
            afterCustomer <- getModel @CrmDomain
            pure (orderDeleted, afterOrder, customerDeleted, afterCustomer)
        result `shouldBe` Right (NoContent, customerModel, NoContent, emptyCrmModel)

    it "returns 500 when a committed customer creation is missing from the model" $ do
        backend <- createForgetful (\model _ -> model) emptyCrmModel
        result <- runCrm backend $ customersServer.create (CreateCustomer "Alice" "alice@example.com")
        either errHTTPCode (const 200) result `shouldBe` 500
        history <- runCrm backend $ getEventList @CrmDomain
        fmap length history `shouldBe` Right 1

    it "returns 500 when a committed order creation is missing from the model" $ do
        backend <- createForgetful (\model _ -> model) customerModel
        result <- runCrm backend $ (ordersServer fixtureCustomerId).create (CreateOrder "Ignored")
        either errHTTPCode (const 200) result `shouldBe` 500
        history <- runCrm backend $ getEventList @CrmDomain
        fmap length history `shouldBe` Right 1
