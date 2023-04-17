{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Action.Counter where

import Control.DeepSeq
import Data.Aeson
import DomainDriven
import GHC.Generics (Generic)
import Prelude

-- | The model, representing the current state
type CounterModel = Int

data CounterEvent
    = CounterIncreased
    | CounterDecreased
    deriving (Show, Generic, ToJSON, FromJSON, NFData)

data CounterAction :: Action where
    GetCounter :: CounterAction x Query Int
    IncreaseCounter :: CounterAction x Cmd Int
    DecreaseCounter :: CounterAction x Cmd Int
    deriving (HasApiOptions)

handleAction
    :: CounterAction 'ParamType method a
    -> HandlerType method CounterModel CounterEvent IO a
handleAction = \case
    GetCounter -> Query $ pure
    IncreaseCounter -> Cmd $ \_ -> pure (id, [CounterIncreased])
    DecreaseCounter -> Cmd $ \_ -> do
        pure (id, [CounterDecreased])

applyCounterEvent :: CounterModel -> Stored CounterEvent -> CounterModel
applyCounterEvent m (Stored event _timestamp _uuid) = case event of
    CounterIncreased -> m + 1
    CounterDecreased -> m - 1

$(mkServerConfig "serverConfig")
