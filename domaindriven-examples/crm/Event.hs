-- | 3-level hierarchical event type: CrmEvent > CustomerEvent > OrderEvent.
--
-- Each level wraps the next with an entity ID, enabling ID-routed dispatch
-- in the event handler.
module Event where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Types
import Prelude

--------------------------------------------------------------------------------
-- Top-level event (what gets stored)
--------------------------------------------------------------------------------

data CrmEvent
    = CustomerEvent
        { customerId :: CustomerId
        , customerEvent :: CustomerEvent
        }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

--------------------------------------------------------------------------------
-- Customer-level events
--------------------------------------------------------------------------------

data CustomerEvent
    = CustomerCreated {name :: Text, email :: Text}
    | CustomerRemoved
    | CustomerNameChanged {name :: Text}
    | CustomerEmailChanged {email :: Text}
    | OrderEvent
        { orderId :: OrderId
        , orderEvent :: OrderEvent
        }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

--------------------------------------------------------------------------------
-- Order-level events (includes item events as a 3rd level)
--------------------------------------------------------------------------------

data OrderEvent
    = OrderCreated {description :: Text}
    | OrderStatusChanged {status :: OrderStatus}
    | OrderDescriptionChanged {description :: Text}
    | OrderRemoved
    | ItemAdded {itemId :: ItemId, productName :: Text, quantity :: Int, unitPrice :: Int}
    | ItemRemoved {itemId :: ItemId}
    | ItemQuantityChanged {itemId :: ItemId, quantity :: Int}
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)
