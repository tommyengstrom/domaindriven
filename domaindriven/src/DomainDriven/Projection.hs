{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module DomainDriven.Projection where

import DomainDriven.Domain
import DomainDriven.Persistance.Class (NoIndex (..), Stored)
import Effectful
import Effectful.Dispatch.Dynamic (send)
import GHC.Stack (HasCallStack)
import Prelude ((.))

-- | The new Projection effect with a single domain parameter
-- We use a proxy to carry the domain type explicitly
data Projection domain :: Effect where
    GetModelI
        :: DomainIndex domain
        -> Projection domain m (DomainModel domain)
    GetEventListI
        :: DomainIndex domain
        -> Projection domain m [Stored (DomainEvent domain)]

--     GetEventStream :: Projection domain m (Stream m (Stored (DomainEvent domain)))

type instance DispatchOf (Projection domain) = 'Dynamic

getModelI
    :: forall domain es model event index
     . ( HasCallStack
       , Projection domain :> es
       , IsDomain domain model event index
       )
    => index
    -> Eff es model
getModelI = send . GetModelI @domain

getEventListI
    :: forall domain es model event index
     . ( HasCallStack
       , Projection domain :> es
       , IsDomain domain model event index
       )
    => index
    -> Eff es [Stored event]
getEventListI = send . GetEventListI @domain

getModel
    :: forall domain es model event
     . ( HasCallStack
       , Projection domain :> es
       , IsDomain domain model event NoIndex
       )
    => Eff es model
getModel = getModelI @domain NoIndex

-- | Get a list of all the events used to create the model (new domain-based API)
getEventList
    :: forall domain es model event
     . ( HasCallStack
       , Projection domain :> es
       , IsDomain domain model event NoIndex
       )
    => Eff es [Stored event]
getEventList = getEventListI @domain NoIndex
