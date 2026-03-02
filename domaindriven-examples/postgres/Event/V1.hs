module Event.V1 where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Prelude

data CounterEvent
    = CounterIncreased
    | CounterDecreased
    deriving (Show, Generic, FromJSON, ToJSON)
