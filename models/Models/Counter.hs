{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.Counter where

import Control.DeepSeq
import Control.Monad (when)
import Control.Monad.Catch
import Data.Aeson
import Data.Typeable (Typeable)
import DomainDriven
import DomainDriven.Config
import GHC.Generics (Generic)
import Prelude

-- | The model, representing the current state
type CounterModel = Int

data CounterEvent
    = CounterIncreased
    | CounterDecreased
    deriving (Show, Generic, ToJSON, FromJSON, NFData)

data CounterAction x method return where
    GetCounter :: CounterAction x Query Int
    IncreaseCounter :: CounterAction x Cmd Int
    DecreaseCounter :: CounterAction x Cmd Int
    AddToCounter
        :: P x "numberToAdd" Int
        -> CounterAction x Cmd Int
        -- ^ Add a positive number to the counter
    deriving (HasApiOptions)

handleAction
    :: CounterAction 'ParamType method a
    -> HandlerType method CounterModel CounterEvent IO a
handleAction = \case
    GetCounter -> Query $ pure
    IncreaseCounter -> Cmd $ \_ -> pure (id, [CounterIncreased])
    DecreaseCounter -> Cmd $ \counter -> do
        when (counter < 1) (throwM NegativeNotSupported)
        pure (id, [CounterDecreased])
    AddToCounter a -> Cmd $ \_ -> do
        when (a < 0) (throwM NegativeNotSupported)
        pure (id, replicate a CounterIncreased)

data CounterError = NegativeNotSupported
    deriving (Show, Eq, Typeable, Exception)

applyCounterEvent :: CounterModel -> Stored CounterEvent -> CounterModel
applyCounterEvent m (Stored event _timestamp _uuid) = case event of
    CounterIncreased -> m + 1
    CounterDecreased -> m - 1

$(mkServerConfig "serverConfig")
