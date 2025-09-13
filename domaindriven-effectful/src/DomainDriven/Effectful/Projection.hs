{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module DomainDriven.Effectful.Projection where

import DomainDriven.Effectful.Domain
import DomainDriven.Persistance.Class (Stored, NoIndex(..))
import Data.Type.Equality
import Effectful
import Effectful.TH

-- | The new Projection effect with a single domain parameter
-- We use a proxy to carry the domain type explicitly
data Projection domain  :: Effect where
    GetModelI :: DomainIndex domain
        -> Projection domain m (DomainModel domain)
    GetEventListI :: DomainIndex domain
        -> Projection domain m [Stored (DomainEvent domain)]
--     GetEventStream :: Projection domain m (Stream m (Stored (DomainEvent domain)))

type instance DispatchOf (Projection domain) = 'Dynamic

$(makeEffect ''Projection)

getModel
    :: forall domain es
     . (DomainIndex domain ~ NoIndex
     , Projection domain :> es
     )
    => Eff es (DomainModel domain)
getModel = getModelI NoIndex

-- | Get a list of all the events used to create the model (new domain-based API)
getEventList
    :: forall domain es
     . (DomainIndex domain ~ NoIndex
     , Projection domain :> es
     )
    => Eff es [Stored (DomainEvent domain)]
getEventList = getEventListI NoIndex

