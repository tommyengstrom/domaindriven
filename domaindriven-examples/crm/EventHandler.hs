{-# LANGUAGE OverloadedRecordDot #-}

-- | Optics-based applyEvent with 3-level hierarchical dispatch.
--
-- Demonstrates:
--   * Using @at key .~ Just val@ for inserts and @at key .~ Nothing@ for deletes
--   * Using @ix key % #field .~ val@ for updates (silently ignored if missing)
--   * ID-routed dispatch: each level extracts an entity ID and delegates
module EventHandler where

import Data.Map.Strict qualified as Map
import DomainDriven (Stored (..))
import Event
import Model (CrmModel (..))
import Data.Function ((&))
import Optics.Core (at, ix, (%))
import Optics.Operators ((.~))
import Types
import Prelude

--------------------------------------------------------------------------------
-- Top-level dispatch
--------------------------------------------------------------------------------

applyEvent :: CrmModel -> Stored CrmEvent -> CrmModel
applyEvent model (Stored (CustomerEvent{customerId = cid, customerEvent = ce}) _ _) =
    applyCustomerEvent cid ce model

--------------------------------------------------------------------------------
-- Customer-level dispatch
--------------------------------------------------------------------------------

applyCustomerEvent :: CustomerId -> CustomerEvent -> CrmModel -> CrmModel
applyCustomerEvent cid ev model = case ev of
    CustomerCreated{name, email} ->
        model{customers =
                Map.insert
                    cid
                    Customer
                        { customerId = cid
                        , name = name
                        , email = email
                        , orders = mempty
                        }
                    model.customers
             }
    CustomerRemoved ->
        model{customers = Map.delete cid model.customers}
    CustomerNameChanged{name} ->
        model{customers = model.customers & ix cid % #name .~ name}
    CustomerEmailChanged{email} ->
        model{customers = model.customers & ix cid % #email .~ email}
    OrderEvent{orderId = oid, orderEvent = oe} ->
        applyOrderEvent cid oid oe model

--------------------------------------------------------------------------------
-- Order-level dispatch
--------------------------------------------------------------------------------

applyOrderEvent :: CustomerId -> OrderId -> OrderEvent -> CrmModel -> CrmModel
applyOrderEvent cid oid ev model =
    let orderPath = ix cid % #orders
    in  case ev of
            OrderCreated{description} ->
                model{customers =
                        model.customers
                            & orderPath
                                % at oid
                                .~ Just
                                    Order
                                        { orderId = oid
                                        , description = description
                                        , status = Pending
                                        , items = mempty
                                        }
                     }
            OrderStatusChanged{status} ->
                model{customers = model.customers & orderPath % ix oid % #status .~ status}
            OrderDescriptionChanged{description} ->
                model{customers = model.customers & orderPath % ix oid % #description .~ description}
            OrderRemoved ->
                model{customers = model.customers & orderPath % at oid .~ Nothing}
            ItemAdded{itemId, productName, quantity, unitPrice} ->
                model{customers =
                        model.customers
                            & orderPath
                                % ix oid
                                % #items
                                % at itemId
                                .~ Just
                                    OrderItem
                                        { itemId = itemId
                                        , productName = productName
                                        , quantity = quantity
                                        , unitPrice = unitPrice
                                        }
                     }
            ItemRemoved{itemId} ->
                model{customers =
                        model.customers & orderPath % ix oid % #items % at itemId .~ Nothing
                     }
            ItemQuantityChanged{itemId, quantity} ->
                model{customers =
                        model.customers
                            & orderPath
                                % ix oid
                                % #items
                                % ix itemId
                                % #quantity
                                .~ quantity
                     }
