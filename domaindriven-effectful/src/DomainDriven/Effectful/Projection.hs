{-# LANGUAGE AllowAmbiguousTypes #-}

module DomainDriven.Effectful.Projection where

import Effectful
import Effectful.Dispatch.Dynamic
import DomainDriven.Persistance.Class (Stored)

data Projection model event index :: Effect where
    GetModel :: Projection model event index m model
    GetEventList ::Projection model event index m [Stored event]

type instance DispatchOf (Projection model event index) = 'Dynamic

-- | Get the model
getModel
    :: forall model event index es
     . Projection model event index :> es
    => Eff es model
getModel = send (GetModel @model @event @index)


-- | Get a list of all the events used to create the model
getEventList
    :: forall model event index es
     . Projection model event index :> es
    => Eff es [Stored event]
getEventList = send (GetEventList @model @event @index)

