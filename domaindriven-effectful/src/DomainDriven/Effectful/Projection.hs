{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module DomainDriven.Effectful.Projection where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Effectful
import Effectful.Dispatch.Dynamic
import DomainDriven.Persistance.Class (Stored)
import DomainDriven.Effectful.Domain

-- | The original Projection effect with three type parameters for backward compatibility
data Projection model event index :: Effect where
    GetModel :: Projection model event index m model
    GetEventList ::Projection model event index m [Stored event]

type instance DispatchOf (Projection model event index) = 'Dynamic

-- | The new Projection effect with a single domain parameter
-- We use a proxy to carry the domain type explicitly
data Projection' (domain :: Type) :: Effect where
    GetModel' :: Proxy domain -> Projection' domain m (DomainModel domain)
    GetEventList' :: Proxy domain -> Projection' domain m [Stored (DomainEvent domain)]

type instance DispatchOf (Projection' domain) = 'Dynamic

-- | Get the model (original API)
getModel
    :: forall model event index es
     . Projection model event index :> es
    => Eff es model
getModel = send (GetModel @model @event @index)

-- | Get a list of all the events used to create the model (original API)
getEventList
    :: forall model event index es
     . Projection model event index :> es
    => Eff es [Stored event]
getEventList = send (GetEventList @model @event @index)

-- | Get the model (new domain-based API)
getModelD
    :: forall domain es
     . Projection' domain :> es
    => Eff es (DomainModel domain)
getModelD = send (GetModel' (Proxy @domain))

-- | Get a list of all the events used to create the model (new domain-based API)
getEventListD
    :: forall domain es
     . Projection' domain :> es
    => Eff es [Stored (DomainEvent domain)]
getEventListD = send (GetEventList' (Proxy @domain))

-- | Type alias to make migration easier - use Projection' with Domain type
type ProjectionD domain = Projection' domain

