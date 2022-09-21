{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Models.Counter where

import           Control.DeepSeq
import           Control.Monad                  ( when )
import           Control.Monad.Catch
import           Data.Aeson
import           Data.Typeable                  ( Typeable )
import           DomainDriven
import           DomainDriven.Config
import           GHC.Generics                   ( Generic )
import           Prelude


-- | The model, representing the current state
type CounterModel = Int

data CounterEvent
    = CounterIncreased
    | CounterDecreased
    deriving (Show, Generic, ToJSON, FromJSON, NFData)

data CounterAction method return where
   GetCounter ::CounterAction Query Int
   IncreaseCounter ::CounterAction Cmd Int
   DecreaseCounter ::CounterAction Cmd Int
   AddToCounter ::Int -> CounterAction Cmd Int -- ^ Add a positive number to the counter
   deriving HasApiOptions

handleAction
    :: CounterAction method a -> HandlerType method CounterModel CounterEvent IO a
handleAction = \case
    GetCounter      -> Query $ pure
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

simpleConfig :: ServerConfig
simpleConfig = ServerConfig mempty mempty

$(mkServerConfig "serverConfig")
