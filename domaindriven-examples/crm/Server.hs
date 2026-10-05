{-# LANGUAGE OverloadedRecordDot #-}

-- | Server handlers demonstrating shared patterns:
--
--   * Explicit capabilities with qualified @Effectful.:>@
--   * @withCustomer@ / @withOrder@ entity handler pattern (composed)
--   * Event wrapping helpers (@wrapCustE@, @wrapOrdE@)
--   * Dual lookup helpers (Eff + Pure variants)
module Server where

import Api
import Command
import Data.Map.Strict qualified as Map
import DomainDriven
import DomainDriven.FieldNameAsPath (FieldNameAsPathServer (..))
import Effectful hiding ((:>))
import Effectful qualified
import Effectful.Error.Static (Error, throwError)
import Event
import Model (CrmDomain, CrmModel (..))
import Servant hiding (throwError)
import Servant.Server.Generic (AsServerT)
import Types
import Prelude

--------------------------------------------------------------------------------
-- Event wrapping helpers — composable chain mirroring the domain hierarchy
--------------------------------------------------------------------------------

wrapCustE :: CustomerId -> CustomerEvent -> CrmEvent
wrapCustE cid ce = CustomerEvent{customerId = cid, customerEvent = ce}

wrapOrdE :: CustomerId -> OrderId -> OrderEvent -> CrmEvent
wrapOrdE cid oid oe = wrapCustE cid OrderEvent{orderId = oid, orderEvent = oe}

--------------------------------------------------------------------------------
-- Lookup helpers — Eff variant (throws 404) + Pure variant (for returnFn)
--------------------------------------------------------------------------------

lookupCustomer :: Error ServerError Effectful.:> es => CustomerId -> CrmModel -> Eff es Customer
lookupCustomer cid m =
    case Map.lookup cid m.customers of
        Just c -> pure c
        Nothing -> throwError err404{errBody = "Customer not found"}

lookupCustomerPure :: CustomerId -> CrmModel -> Either ServerError Customer
lookupCustomerPure cid m =
    maybe (Left err500) Right (Map.lookup cid m.customers)

lookupOrder :: Error ServerError Effectful.:> es => Customer -> OrderId -> Eff es Order
lookupOrder cust oid =
    case Map.lookup oid cust.orders of
        Just o -> pure o
        Nothing -> throwError err404{errBody = "Order not found"}

lookupOrderPure :: CustomerId -> OrderId -> CrmModel -> Either ServerError Order
lookupOrderPure cid oid m = do
    cust <- lookupCustomerPure cid m
    maybe (Left err500) Right (Map.lookup oid cust.orders)

--------------------------------------------------------------------------------
-- Entity handler patterns — withCustomer, withOrder (composed)
--------------------------------------------------------------------------------

-- | Look up customer, 404 if missing, run callback, return updated customer.
withCustomer
    :: (Aggregate CrmDomain Effectful.:> es, Error ServerError Effectful.:> es)
    => CustomerId
    -> (Customer -> Eff es [CrmEvent])
    -> Eff es Customer
withCustomer cid mkEvents = do
    result <- runTransaction @CrmDomain \m -> do
        cust <- lookupCustomer cid m
        evts <- mkEvents cust
        pure (lookupCustomerPure cid, evts)
    either throwError pure result

-- | Look up customer and order in one transaction, 404 if either is missing.
withOrder
    :: (Aggregate CrmDomain Effectful.:> es, Error ServerError Effectful.:> es)
    => CustomerId
    -> OrderId
    -> (Customer -> Order -> Eff es [CrmEvent])
    -> Eff es Order
withOrder cid oid mkEvents = do
    result <- runTransaction @CrmDomain \m -> do
        cust <- lookupCustomer cid m
        ord <- lookupOrder cust oid
        evts <- mkEvents cust ord
        pure (lookupOrderPure cid oid, evts)
    either throwError pure result

--------------------------------------------------------------------------------
-- Server implementation
--------------------------------------------------------------------------------

customersServer
    :: ( Projection CrmDomain Effectful.:> es
       , Aggregate CrmDomain Effectful.:> es
       , Error ServerError Effectful.:> es
       , GenId Effectful.:> es
       )
    => CustomersApi (AsServerT (Eff es))
customersServer =
    CustomersApi
        { list_ = do
            CrmModel{customers} <- getModel @CrmDomain
            pure $ Map.elems customers
        , create = \cmd -> do
            result <- runTransaction @CrmDomain \_ -> do
                cid <- CustomerId <$> genId
                let evts :: [CrmEvent]
                    evts = [wrapCustE cid CustomerCreated{name = cmd.name, email = cmd.email}]
                pure (lookupCustomerPure cid, evts)
            either throwError pure result
        , detail = \cid ->
            FieldNameAsPathServer $ customerServer cid
        }

customerServer
    :: ( Projection CrmDomain Effectful.:> es
       , Aggregate CrmDomain Effectful.:> es
       , Error ServerError Effectful.:> es
       , GenId Effectful.:> es
       )
    => CustomerId -> CustomerApi (AsServerT (Eff es))
customerServer cid =
    CustomerApi
        { get_ = do
            m <- getModel @CrmDomain
            lookupCustomer cid m
        , changeName = \cmd ->
            withCustomer cid \_cust ->
                pure [wrapCustE cid CustomerNameChanged{name = cmd.name}]
        , changeEmail = \cmd ->
            withCustomer cid \_cust ->
                pure [wrapCustE cid CustomerEmailChanged{email = cmd.email}]
        , remove = runTransaction @CrmDomain \m -> do
            _ <- lookupCustomer cid m
            pure (const NoContent, [wrapCustE cid CustomerRemoved])
        , orders = FieldNameAsPathServer $ ordersServer cid
        }

ordersServer
    :: ( Projection CrmDomain Effectful.:> es
       , Aggregate CrmDomain Effectful.:> es
       , Error ServerError Effectful.:> es
       , GenId Effectful.:> es
       )
    => CustomerId -> OrdersApi (AsServerT (Eff es))
ordersServer cid =
    OrdersApi
        { list_ = do
            m <- getModel @CrmDomain
            cust <- lookupCustomer cid m
            pure $ Map.elems cust.orders
        , create = \cmd -> do
            result <- runTransaction @CrmDomain \m -> do
                _ <- lookupCustomer cid m
                oid <- OrderId <$> genId
                pure
                    ( lookupOrderPure cid oid
                    , [wrapOrdE cid oid OrderCreated{description = cmd.description}]
                    )
            either throwError pure result
        , detail = \oid ->
            FieldNameAsPathServer $ orderServer cid oid
        }

orderServer
    :: ( Projection CrmDomain Effectful.:> es
       , Aggregate CrmDomain Effectful.:> es
       , Error ServerError Effectful.:> es
       , GenId Effectful.:> es
       )
    => CustomerId -> OrderId -> OrderApi (AsServerT (Eff es))
orderServer cid oid =
    OrderApi
        { get_ = do
            m <- getModel @CrmDomain
            cust <- lookupCustomer cid m
            lookupOrder cust oid
        , changeStatus = \cmd ->
            withOrder cid oid \_ _ ->
                pure [wrapOrdE cid oid OrderStatusChanged{status = cmd.status}]
        , changeDescription = \cmd ->
            withOrder cid oid \_ _ ->
                pure [wrapOrdE cid oid OrderDescriptionChanged{description = cmd.description}]
        , remove = runTransaction @CrmDomain \m -> do
            cust <- lookupCustomer cid m
            _ <- lookupOrder cust oid
            pure (const NoContent, [wrapOrdE cid oid OrderRemoved])
        , addItem = \cmd -> do
            iid <- ItemId <$> genId
            withOrder cid oid \_ _ ->
                pure
                    [ wrapOrdE
                        cid
                        oid
                        ItemAdded
                            { itemId = iid
                            , productName = cmd.productName
                            , quantity = cmd.quantity
                            , unitPrice = cmd.unitPrice
                            }
                    ]
        , removeItem = \iid ->
            withOrder cid oid \_ _ ->
                pure [wrapOrdE cid oid ItemRemoved{itemId = iid}]
        , changeItemQuantity = \iid cmd ->
            withOrder cid oid \_ _ ->
                pure [wrapOrdE cid oid ItemQuantityChanged{itemId = iid, quantity = cmd.quantity}]
        }
