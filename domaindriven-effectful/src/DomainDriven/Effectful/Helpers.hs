{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FunctionalDependencies #-}

module DomainDriven.Effectful.Helpers where

import DomainDriven.Effectful.Aggregate
import DomainDriven.Effectful.Projection
import DomainDriven.Effectful.Domain
import DomainDriven.Persistance.Class (Stored, NoIndex(..))
import Effectful
import Prelude

-- | Type class to associate events and indices with models
-- This uses functional dependencies to reduce type parameters
class EventSourced model event index | model -> event index where
    -- This class just establishes the relationship

-- | Helper for running transactions on NoIndex aggregates
withAggregate
    :: forall model event es a
     . Aggregate model event NoIndex :> es
    => (model -> Eff es (model -> a, [event]))
    -> Eff es a
withAggregate = runTransaction @model @event @NoIndex NoIndex

-- | Helper for querying NoIndex projections
queryModel
    :: forall model event es
     . Projection model event NoIndex :> es
    => Eff es model
queryModel = getModel @model @event @NoIndex

-- | Helper for getting events from NoIndex projections
queryEvents
    :: forall model event es
     . Projection model event NoIndex :> es
    => Eff es [Stored event]
queryEvents = getEventList @model @event @NoIndex

-- | Convenience function for simple commands that just emit events
simpleCommand
    :: forall model event es a
     . Aggregate model event NoIndex :> es
    => (model -> a)        -- ^ How to extract result from updated model
    -> [event]             -- ^ Events to emit
    -> Eff es a
simpleCommand getResult events = 
    withAggregate @model @event $ \_ -> pure (getResult, events)

-- | Convenience function for commands that check the model before emitting events
conditionalCommand
    :: forall model event es a
     . Aggregate model event NoIndex :> es
    => (model -> Maybe [event])  -- ^ Check model and optionally produce events
    -> (model -> a)              -- ^ Extract result from model
    -> Eff es (Maybe a)
conditionalCommand checkModel getResult =
    withAggregate @model @event $ \model ->
        case checkModel model of
            Nothing -> pure (const Nothing, [])
            Just events -> pure (Just . getResult, events)

-- ============================================================================
-- Domain-based helper functions (using single type parameter)
-- ============================================================================

-- | Helper for running transactions on domains
withAggregateD
    :: forall domain es a
     . ( Aggregate' domain :> es
       , DomainIndex domain ~ NoIndex
       )
    => (DomainModel domain -> Eff es (DomainModel domain -> a, [DomainEvent domain]))
    -> Eff es a
withAggregateD = runTransactionD @domain NoIndex

-- | Helper for querying domain projections
queryModelD
    :: forall domain es
     . Projection' domain :> es
    => Eff es (DomainModel domain)
queryModelD = getModelD @domain

-- | Helper for getting events from domain projections
queryEventsD
    :: forall domain es
     . Projection' domain :> es
    => Eff es [Stored (DomainEvent domain)]
queryEventsD = getEventListD @domain

-- | Convenience function for simple commands that just emit events (domain version)
simpleCommandD
    :: forall domain es a
     . ( Aggregate' domain :> es
       , DomainIndex domain ~ NoIndex
       )
    => (DomainModel domain -> a)        -- ^ How to extract result from updated model
    -> [DomainEvent domain]             -- ^ Events to emit
    -> Eff es a
simpleCommandD getResult events = 
    withAggregateD @domain $ \_ -> pure (getResult, events)

-- | Convenience function for commands that check the model before emitting events (domain version)
conditionalCommandD
    :: forall domain es a
     . ( Aggregate' domain :> es
       , DomainIndex domain ~ NoIndex
       )
    => (DomainModel domain -> Maybe [DomainEvent domain])  -- ^ Check model and optionally produce events
    -> (DomainModel domain -> a)                           -- ^ Extract result from model
    -> Eff es (Maybe a)
conditionalCommandD checkModel getResult =
    withAggregateD @domain $ \model ->
        case checkModel model of
            Nothing -> pure (const Nothing, [])
            Just events -> pure (Just . getResult, events)