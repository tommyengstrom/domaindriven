-- | Request body types (commands).
--
-- Commands represent what the client *wants* to happen. They are separate from
-- events, which represent what *did* happen. Each mutation endpoint gets its
-- own command type.
module Command where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Types (OrderStatus)
import Prelude

-- Customer commands
data CreateCustomer = CreateCustomer
    { name :: Text
    , email :: Text
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

newtype ChangeCustomerName = ChangeCustomerName {name :: Text}
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

newtype ChangeCustomerEmail = ChangeCustomerEmail {email :: Text}
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- Order commands
newtype CreateOrder = CreateOrder {description :: Text}
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

newtype ChangeOrderStatus = ChangeOrderStatus {status :: OrderStatus}
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

newtype ChangeOrderDescription = ChangeOrderDescription {description :: Text}
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- Item commands
data AddItem = AddItem
    { productName :: Text
    , quantity :: Int
    , unitPrice :: Int
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

newtype ChangeItemQuantity = ChangeItemQuantity {quantity :: Int}
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)
