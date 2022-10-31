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

data CounterAction x method return where
   GetCounter      ::CounterAction x Query Int
   GetCounterPlus  ::P x "int" Int
                   -> CounterAction x Query Int
   IncreaseCounter ::CounterAction x Cmd Int
   AddToCounter    ::P x "int" Int
                   -> CounterAction x Cmd Int
   AdvancedAction  ::P x "int" Int
                   -> P x "advanced" (AdvancedAction x m r)
                   -> CounterAction x m r
   deriving HasApiOptions

data AdvancedAction x method return where
    AdvancedIncrease ::P x "int1" Int
                     -> P x "int2" Int
                     -> AdvancedAction x Cmd Int
    AdvancedGet      ::P x "int1" Int
                     -> P x "int2" Int
                     ->  AdvancedAction x Query Int
   deriving HasApiOptions

handleCmd
    :: CounterAction 'ParamType method a
    -> HandlerType method CounterModel CounterEvent IO a
handleCmd = \case
    GetCounter         -> Query $ pure
    GetCounterPlus i   -> Query $ pure . (+ i)
    IncreaseCounter    -> Cmd $ \_ -> pure (id, [CounterIncreased])
    AddToCounter a     -> Cmd $ \_ -> pure (id, replicate a CounterIncreased)
    AdvancedAction i a -> handleAdvancedAction i a

handleAdvancedAction
    :: Int -> ActionHandler CounterModel CounterEvent IO (AdvancedAction 'ParamType)
handleAdvancedAction i = \case
    AdvancedIncrease _ _ -> Cmd $ \_ -> pure (id, replicate i CounterIncreased)
    AdvancedGet      a b -> Query $ \m -> pure $ m * i + a * b

data CounterError = NegativeNotSupported
    deriving (Show, Eq, Typeable, Exception)

applyCounterEvent :: CounterModel -> Stored CounterEvent -> CounterModel
applyCounterEvent m (Stored event _timestamp _uuid) = case event of
    CounterIncreased -> m + 1
    CounterDecreased -> m - 1

simpleConfig :: ServerConfig
simpleConfig = ServerConfig mempty

$(mkServerConfig "serverConfig")
