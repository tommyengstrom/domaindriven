module Event.V2 where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Prelude

data CounterEvent
    = CounterIncreasedBy Int
    | CounterDecreasedBy Int
    deriving (Show, Generic, FromJSON, ToJSON)
