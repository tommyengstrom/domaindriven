module Event.V1 where

import Data.Text (Text)
import Data.UUID (UUID)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Prelude

type UserId = UUID

data Event
    = UserEvent UserId UserEvent
    | InventoryEvent InventoryEvent
    deriving (Show, Generic, FromJSON, ToJSON)

data UserEvent
    = UserCreated {userName :: Text}
    | UserNameChanged {newUserName :: Text}
    deriving (Show, Generic, FromJSON, ToJSON)

data InventoryEvent
    = ItemAdded
        { itemId :: UUID
        , itemName :: Text
        , quantity :: Int
        }
    | ItemRenamed {itemId :: UUID, itemName :: Text}
    deriving (Show, Generic, FromJSON, ToJSON)
