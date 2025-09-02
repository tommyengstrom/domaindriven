{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module DomainDriven.Effectful.Domain
    ( -- * Domain configuration
      Domain(..)
    , DomainModel
    , DomainEvent
    , DomainIndex
    , SimpleDomain
    ) where

import Data.Kind (Type)
import DomainDriven.Persistance.Class (NoIndex)

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

-- | A simplified domain for the common case of NoIndex
type SimpleDomain model event = Domain model event NoIndex