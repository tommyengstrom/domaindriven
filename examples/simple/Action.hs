{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Action where

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
    deriving (Show, Generic, ToJSON, FromJSON)

data CounterCmd method return where
   GetCounter ::CounterCmd Query Int
   IncreaseCounter ::CounterCmd Cmd Int
   DecreaseCounter ::CounterCmd Cmd Int
   AddToCounter ::Int -> CounterCmd Cmd Int -- ^ Add a positive number to the counter
   deriving HasApiOptions

handleCmd :: CounterCmd method a -> HandlerType method CounterModel CounterEvent IO a
handleCmd = \case
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
