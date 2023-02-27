{-# LANGUAGE TemplateHaskell #-}

module Counter where

import Data.Aeson
import DomainDriven
import DomainDriven.Persistance.Postgres qualified as PG
import GHC.Generics (Generic)
import Servant
import Servant.Server.Generic
import ServantPatch
import Prelude

type WM = PG.PostgresEvent CounterModel CounterEvent

-- writeModel =

-- | The model, representing the current state
type CounterModel = Int

data CounterEvent
    = CounterIncreased
    | CounterDecreased
    deriving (Show, Generic, ToJSON, FromJSON)

data CounterAction route = CounterAction
    { get :: route :- Cmd' (PG.PostgresEvent CounterModel CounterEvent) (Get '[JSON] Int)
    }

counterServer :: CounterAction AsServer
counterServer =
    CounterAction
        { get = \state -> pure (const state, [])
        }
