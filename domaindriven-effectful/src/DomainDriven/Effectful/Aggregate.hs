{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module DomainDriven.Effectful.Aggregate where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Effectful
import Effectful.Dispatch.Dynamic
import DomainDriven.Effectful.Domain

-- | The original Aggregate effect with three type parameters for backward compatibility
data Aggregate model event index :: Effect where
    RunTransaction
        :: index
        -> (model -> Eff es (model -> a, [event]))
        -> Aggregate model event index (Eff es) a

type instance DispatchOf (Aggregate model event index) = 'Dynamic

-- | The new Aggregate effect with a single domain parameter
data Aggregate' (domain :: Type) :: Effect where
    RunTransaction'
        :: Proxy domain
        -> DomainIndex domain
        -> (DomainModel domain -> Eff es (DomainModel domain -> a, [DomainEvent domain]))
        -> Aggregate' domain (Eff es) a

type instance DispatchOf (Aggregate' domain) = 'Dynamic

-- | Run a synchronous transaction while holding a lock on the aggregate (original API)
runTransaction
    :: forall model event index es a
     . Aggregate model event index :> es
    => index
    -> (model -> Eff es (model -> a, [event]))
    -> Eff es a
runTransaction idx cmd = send (RunTransaction idx cmd)

-- | Run a synchronous transaction while holding a lock on the aggregate (new domain-based API)
runTransactionD
    :: forall domain es a
     . Aggregate' domain :> es
    => DomainIndex domain
    -> (DomainModel domain -> Eff es (DomainModel domain -> a, [DomainEvent domain]))
    -> Eff es a
runTransactionD idx cmd = send (RunTransaction' (Proxy @domain) idx cmd)

-- | Type alias to make migration easier - use Aggregate' with Domain type
type AggregateD domain = Aggregate' domain
