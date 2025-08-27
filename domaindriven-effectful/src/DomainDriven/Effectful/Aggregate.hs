{-# LANGUAGE AllowAmbiguousTypes #-}

module DomainDriven.Effectful.Aggregate where

import Effectful
import Effectful.Dispatch.Dynamic

data Aggregate model event index :: Effect where
    RunTransaction
        :: index
        -> (model -> Eff es (model -> a, [event]))
        -> Aggregate model event index (Eff es) a

type instance DispatchOf (Aggregate model event index) = 'Dynamic

-- | Run a synchronous transaction while holding a lock on the aggregate
runTransaction
    :: forall model event index es a
     . Aggregate model event index :> es
    => index
    -> (model -> Eff es (model -> a, [event]))
    -> Eff es a
runTransaction idx cmd = send (RunTransaction idx cmd)
