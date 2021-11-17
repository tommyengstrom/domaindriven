{-# LANGUAGE TemplateHaskell #-}
module App where

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
    | SetTo Int
    deriving (Show, Generic, ToJSON, FromJSON)

data CounterCmd method return where
   GetCounter ::CounterCmd Query Int
   IncreaseCounter ::CounterCmd Cmd Int
   AddToCounter ::{ amount :: Int } ->  CounterCmd Cmd Int
   Overengineered ::{ factor :: Int
                     , offset :: Int
                     } -> CounterCmd Cmd Int
   deriving HasApiOptions

handleCmd :: CounterCmd method a -> HandlerType method CounterModel CounterEvent a
handleCmd = \case
    GetCounter         -> Query $ pure
    IncreaseCounter    -> Cmd $ \m -> pure (m + 1, [CounterIncreased])
    AddToCounter a     -> Cmd $ \m -> pure (m + a, replicate a CounterIncreased)
    Overengineered a o -> Cmd $ \m -> let x = a * m + o in pure (x, [SetTo x])

data CounterError = NegativeNotSupported
    deriving (Show, Eq, Typeable, Exception)

applyCounterEvent :: CounterModel -> Stored CounterEvent -> CounterModel
applyCounterEvent m (Stored event _timestamp _uuid) = case event of
    CounterIncreased -> m + 1
    CounterDecreased -> m - 1
    SetTo i          -> i

$(mkServerConfig "serverConfig")
