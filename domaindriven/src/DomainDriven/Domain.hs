{-# LANGUAGE UndecidableInstances #-}

module DomainDriven.Domain
    ( module DomainDriven.Domain
    ) where

import Data.Kind (Type)
import Data.Type.Equality (type (~))

-- | A domain configuration that bundles model, event, and index types
data Domain (model :: Type) (event :: Type) (index :: Type) = Domain

-- | Extract the model type from a domain
type family DomainModel domain where
    DomainModel (Domain m e i) = m

-- | Extract the event type from a domain
type family DomainEvent domain where
    DomainEvent (Domain m e i) = e

-- | Extract the index type from a domain
type family DomainIndex domain where
    DomainIndex (Domain m e i) = i

-- | Relates a domain to its components as a class constraint rather than as
-- type family equalities. effectful-plugin only picks the 'Domain' of an
-- effect when the remaining constraints mentioning it are class constraints
-- with a matching instance, so equalities on 'DomainModel' and friends would
-- leave the domain ambiguous at every call site.
class
    ( DomainModel domain ~ model
    , DomainEvent domain ~ event
    , DomainIndex domain ~ index
    ) =>
    IsDomain domain model event index
        | domain -> model event index

instance
    (m ~ model, e ~ event, i ~ index)
    => IsDomain (Domain m e i) model event index
