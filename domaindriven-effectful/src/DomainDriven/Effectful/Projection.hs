{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module DomainDriven.Effectful.Projection where

import Data.Kind (Type)
import DomainDriven.Effectful.Domain
import DomainDriven.Persistance.Class (Stored)
import Effectful
import Effectful.Dispatch.Dynamic

-- | The new Projection effect with a single domain parameter
-- We use a proxy to carry the domain type explicitly
data Projection (domain :: Type) :: Effect where
    GetModel :: Projection domain m (DomainModel domain)
    GetEventList :: Projection domain m [Stored (DomainEvent domain)]

type instance DispatchOf (Projection domain) = 'Dynamic

-- | Get the model (new domain-based API)
getModel
    :: forall domain es
     . Projection domain :> es
    => Eff es (DomainModel domain)
getModel = send (GetModel @domain)

-- | Get a list of all the events used to create the model (new domain-based API)
getEventList
    :: forall domain es
     . Projection domain :> es
    => Eff es [Stored (DomainEvent domain)]
getEventList = send (GetEventList @domain)

