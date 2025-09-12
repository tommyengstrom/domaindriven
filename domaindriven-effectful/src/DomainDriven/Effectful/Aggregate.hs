{-# LANGUAGE AllowAmbiguousTypes #-}

module DomainDriven.Effectful.Aggregate where

import Data.Kind (Type)
import DomainDriven.Effectful.Domain
import Effectful
import Effectful.Dispatch.Dynamic

-- | The new Aggregate effect with a single domain parameter
data Aggregate (domain :: Type) :: Effect where
    RunTransaction
        :: DomainIndex domain
        -> ( DomainModel domain
             -> Eff
                    es
                    ( DomainModel domain -> a
                    , [DomainEvent domain]
                    )
           )
        -> Aggregate domain (Eff es) a

type instance DispatchOf (Aggregate domain) = 'Dynamic

-- | Run a synchronous transaction while holding a lock on the aggregate
-- The returnd value is a projection of the model after the events have been applied.
runTransaction
    :: forall domain es a
     . Aggregate domain :> es
    => DomainIndex domain
    -> ( DomainModel domain
         -> Eff
                es
                ( DomainModel domain -> a
                , [DomainEvent domain]
                )
       )
    -> Eff es a
runTransaction idx cmd = send (RunTransaction @domain idx cmd)
