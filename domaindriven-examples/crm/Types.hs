-- | Domain primitives — ID newtypes and enumerations.
module Types where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Servant (FromHttpApiData)
import Prelude

--------------------------------------------------------------------------------
-- ID types
--------------------------------------------------------------------------------

newtype CustomerId = CustomerId UUID
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (FromJSON, ToJSON, FromJSONKey, ToJSONKey, FromHttpApiData)

newtype OrderId = OrderId UUID
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (FromJSON, ToJSON, FromJSONKey, ToJSONKey, FromHttpApiData)

newtype ItemId = ItemId UUID
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (FromJSON, ToJSON, FromJSONKey, ToJSONKey, FromHttpApiData)

--------------------------------------------------------------------------------
-- Enumerations
--------------------------------------------------------------------------------

data OrderStatus = Pending | Confirmed | Shipped | Cancelled
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

--------------------------------------------------------------------------------
-- Entity types
--------------------------------------------------------------------------------

data Customer = Customer
    { customerId :: CustomerId
    , name :: Text
    , email :: Text
    , orders :: Map OrderId Order
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON)

data Order = Order
    { orderId :: OrderId
    , description :: Text
    , status :: OrderStatus
    , items :: Map ItemId OrderItem
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON)

data OrderItem = OrderItem
    { itemId :: ItemId
    , productName :: Text
    , quantity :: Int
    , unitPrice :: Int
    -- ^ Price in cents
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON)
