{-# LANGUAGE OverloadedRecordDot #-}

-- | Server handlers demonstrating shared patterns:
--
--   * @Effects@ type alias with qualified @Effectful.:>@
--   * @withCustomer@ / @withOrder@ entity handler pattern (composed)
--   * Event wrapping helpers (@wrapCustE@, @wrapOrdE@)
--   * Dual lookup helpers (Eff + Pure variants)
module Server where

import Api
import Command
import Data.Map.Strict qualified as Map
import DomainDriven
import DomainDriven.FieldNameAsPath (FieldNameAsPathServer (..))
import DomainDriven.Persistance.Class (mkId)
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
-- Effects type alias — qualified Effectful.:> avoids Servant collision
--------------------------------------------------------------------------------

type Effects es =
    ( Projection CrmDomain Effectful.:> es
    , Aggregate CrmDomain Effectful.:> es
    , Error ServerError Effectful.:> es
    , IOE Effectful.:> es
    )

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

lookupCustomerPure :: CustomerId -> CrmModel -> Customer
lookupCustomerPure cid m =
    case Map.lookup cid m.customers of
        Just c -> c
        Nothing -> error "Invariant violation: customer not found after transaction"

lookupOrder :: Error ServerError Effectful.:> es => Customer -> OrderId -> Eff es Order
lookupOrder cust oid =
    case Map.lookup oid cust.orders of
        Just o -> pure o
        Nothing -> throwError err404{errBody = "Order not found"}

lookupOrderPure :: CustomerId -> OrderId -> CrmModel -> Order
lookupOrderPure cid oid m =
    let cust = lookupCustomerPure cid m
    in  case Map.lookup oid cust.orders of
            Just o -> o
            Nothing -> error "Invariant violation: order not found after transaction"

--------------------------------------------------------------------------------
-- Entity handler patterns — withCustomer, withOrder (composed)
--------------------------------------------------------------------------------

-- | Look up customer, 404 if missing, run callback, return updated customer.
withCustomer
    :: Effects es
    => CustomerId
    -> (Customer -> Eff es [CrmEvent])
    -> Eff es Customer
withCustomer cid mkEvents = runTransaction @CrmDomain \m -> do
    cust <- lookupCustomer cid m
    evts <- mkEvents cust
    pure (\m' -> lookupCustomerPure cid m', evts)

-- | Composed: withOrder delegates to withCustomer's transaction.
-- Looks up both customer and order, 404 if either missing.
withOrder
    :: Effects es
    => CustomerId
    -> OrderId
    -> (Customer -> Order -> Eff es [CrmEvent])
    -> Eff es Order
withOrder cid oid mkEvents = runTransaction @CrmDomain \m -> do
    cust <- lookupCustomer cid m
    ord <- lookupOrder cust oid
    evts <- mkEvents cust ord
    pure (\m' -> lookupOrderPure cid oid m', evts)

--------------------------------------------------------------------------------
-- Server implementation
--------------------------------------------------------------------------------

customersServer :: Effects es => CustomersApi (AsServerT (Eff es))
customersServer =
    CustomersApi
        { list_ = do
            CrmModel{customers} <- getModel @CrmDomain
            pure $ Map.elems customers
        , create = \cmd -> runTransaction @CrmDomain \_ -> do
            cid <- CustomerId <$> liftIO mkId
            let evts = [wrapCustE cid CustomerCreated{name = cmd.name, email = cmd.email}]
            pure (\m' -> lookupCustomerPure cid m', evts)
        , detail = \cid ->
            FieldNameAsPathServer $ customerServer cid
        }

customerServer :: Effects es => CustomerId -> CustomerApi (AsServerT (Eff es))
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
        , remove = do
            _ <- withCustomer cid \_cust -> pure [wrapCustE cid CustomerRemoved]
            pure NoContent
        , orders = FieldNameAsPathServer $ ordersServer cid
        }

ordersServer :: Effects es => CustomerId -> OrdersApi (AsServerT (Eff es))
ordersServer cid =
    OrdersApi
        { list_ = do
            m <- getModel @CrmDomain
            cust <- lookupCustomer cid m
            pure $ Map.elems cust.orders
        , create = \cmd -> do
            oid <- OrderId <$> liftIO mkId
            _ <-
                withCustomer cid \_cust ->
                    pure [wrapOrdE cid oid OrderCreated{description = cmd.description}]
            m' <- getModel @CrmDomain
            pure $ lookupOrderPure cid oid m'
        , detail = \oid ->
            FieldNameAsPathServer $ orderServer cid oid
        }

orderServer :: Effects es => CustomerId -> OrderId -> OrderApi (AsServerT (Eff es))
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
        , remove = do
            _ <- withOrder cid oid \_ _ -> pure [wrapOrdE cid oid OrderRemoved]
            pure NoContent
        , addItem = \cmd -> do
            iid <- ItemId <$> liftIO mkId
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
