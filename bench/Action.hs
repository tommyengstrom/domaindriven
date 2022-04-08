{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Action where

import           Control.DeepSeq                ( NFData )
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

data CounterCmd method return where
   GetCounter ::CounterCmd Query Int
   GetCounterPlus ::Int -> CounterCmd Query Int
   IncreaseCounter ::CounterCmd Cmd Int
   AddToCounter ::Int -> CounterCmd Cmd Int
   deriving HasApiOptions

handleCmd :: CounterCmd method a -> HandlerType method CounterModel CounterEvent IO a
handleCmd = \case
    GetCounter       -> Query $ pure
    GetCounterPlus i -> Query $ pure . (+ i)
    IncreaseCounter  -> Cmd $ \_ -> pure (id, [CounterIncreased])
    AddToCounter a   -> Cmd $ \_ -> pure (id, replicate a CounterIncreased)

data CounterError = NegativeNotSupported
    deriving (Show, Eq, Typeable, Exception)

applyCounterEvent :: CounterModel -> Stored CounterEvent -> CounterModel
applyCounterEvent m (Stored event _timestamp _uuid) = case event of
    CounterIncreased -> m + 1
    CounterDecreased -> m - 1

simpleConfig :: ServerConfig
simpleConfig = ServerConfig mempty mempty

$(mkServerConfig "serverConfig")
