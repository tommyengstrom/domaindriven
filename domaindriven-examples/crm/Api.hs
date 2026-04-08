-- | Nested FieldNameAsPath API with Captures at each level.
--
-- Demonstrates:
--   * list_/create/detail pattern at collection level
--   * Capture + FieldNameAsPathApi for nesting
--   * get_ for single-entity retrieval (underscore avoids Prelude clash)
--   * 3 levels of nesting: Customers > Customer > Orders > Order
module Api where

import Command
import DomainDriven.FieldNameAsPath
import GHC.Generics (Generic)
import Servant
import Types

--------------------------------------------------------------------------------
-- Top-level: /customers
--------------------------------------------------------------------------------

data CustomersApi mode = CustomersApi
    { list_ :: mode :- Get '[JSON] [Customer]
    , create :: mode :- ReqBody '[JSON] CreateCustomer :> Post '[JSON] Customer
    , detail :: mode :- Capture "customerId" CustomerId :> FieldNameAsPathApi CustomerApi
    }
    deriving stock (Generic)

instance ApiTagFromLabel CustomersApi

--------------------------------------------------------------------------------
-- Single customer: /customers/{customerId}/...
--------------------------------------------------------------------------------

data CustomerApi mode = CustomerApi
    { get_ :: mode :- Get '[JSON] Customer
    , changeName :: mode :- ReqBody '[JSON] ChangeCustomerName :> Post '[JSON] Customer
    , changeEmail :: mode :- ReqBody '[JSON] ChangeCustomerEmail :> Post '[JSON] Customer
    , remove :: mode :- Delete '[JSON] NoContent
    , orders :: mode :- FieldNameAsPathApi OrdersApi
    }
    deriving stock (Generic)

instance ApiTagFromLabel CustomerApi

--------------------------------------------------------------------------------
-- Orders collection: /customers/{customerId}/orders/...
--------------------------------------------------------------------------------

data OrdersApi mode = OrdersApi
    { list_ :: mode :- Get '[JSON] [Order]
    , create :: mode :- ReqBody '[JSON] CreateOrder :> Post '[JSON] Order
    , detail :: mode :- Capture "orderId" OrderId :> FieldNameAsPathApi OrderApi
    }
    deriving stock (Generic)

instance ApiTagFromLabel OrdersApi

--------------------------------------------------------------------------------
-- Single order: /customers/{customerId}/orders/{orderId}/...
--------------------------------------------------------------------------------

data OrderApi mode = OrderApi
    { get_ :: mode :- Get '[JSON] Order
    , changeStatus :: mode :- ReqBody '[JSON] ChangeOrderStatus :> Post '[JSON] Order
    , changeDescription :: mode :- ReqBody '[JSON] ChangeOrderDescription :> Post '[JSON] Order
    , remove :: mode :- Delete '[JSON] NoContent
    , addItem :: mode :- ReqBody '[JSON] AddItem :> Post '[JSON] Order
    , removeItem :: mode :- Capture "itemId" ItemId :> "remove" :> Post '[JSON] Order
    , changeItemQuantity
        :: mode
            :- Capture "itemId" ItemId
                :> "changeQuantity"
                :> ReqBody '[JSON] ChangeItemQuantity
                :> Post '[JSON] Order
    }
    deriving stock (Generic)

instance ApiTagFromLabel OrderApi
