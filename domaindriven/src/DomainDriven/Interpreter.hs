module DomainDriven.Interpreter
    ( runAggregate
    , runGenId
    , runGenIdWith
    , runProjection
    , runSubAggregateI
    , runSubProjectionI
    , runSubDomainI
    , runSubAggregate
    , runSubProjection
    , runSubDomain
    ) where

import Data.Maybe (mapMaybe)
import DomainDriven.Aggregate
import DomainDriven.Domain
import DomainDriven.GenId
import DomainDriven.Projection
import DomainDriven.Persistance.Class (NoIndex (..), ReadModel, WriteModel)
import DomainDriven.Persistance.Class qualified as P
import Data.UUID (UUID)
import Effectful
import Effectful.Dispatch.Dynamic
import Prelude

-- | Run 'GenId' using the core random UUID generator.
runGenId
    :: IOE :> es
    => Eff (GenId : es) a
    -> Eff es a
runGenId = runGenIdWith $ liftIO P.mkId

-- | Run 'GenId' using a caller-provided UUID action.
--
-- The action is evaluated once for every call to 'genId', which allows tests
-- to supply fixed or stateful deterministic generators without adding 'IOE'
-- to application code.
runGenIdWith
    :: Eff es UUID
    -> Eff (GenId : es) a
    -> Eff es a
runGenIdWith generate = interpret $ \_ -> \case
    GenId -> generate

-- | Run the 'Projection' effect using any 'ReadModel' backend.
runProjection
    :: forall backend domain es a
     . ( IOE :> es
       , ReadModel backend
       , P.Model backend ~ DomainModel domain
       , P.Event backend ~ DomainEvent domain
       , P.Index backend ~ DomainIndex domain
       )
    => backend
    -> Eff (Projection domain : es) a
    -> Eff es a
runProjection backend = interpret $ \_ -> \case
    GetModelI idx -> liftIO $ P.getModel backend idx
    GetEventListI idx -> liftIO $ P.getEventList backend idx

-- | Run the 'Aggregate' effect using any 'WriteModel' backend.
--
-- Delegates to 'P.runCmd' which fires 'postUpdateHook' asynchronously
-- after each transactional update, ensuring uniform hook behavior
-- regardless of backend.
runAggregate
    :: forall backend domain es a
     . ( IOE :> es
       , WriteModel backend
       , P.Model backend ~ DomainModel domain
       , P.Event backend ~ DomainEvent domain
       , P.Index backend ~ DomainIndex domain
       )
    => backend
    -> Eff (Aggregate domain : es) a
    -> Eff es a
runAggregate backend = interpret $ \env -> \case
    RunTransactionI idx cmd ->
        localSeqUnlift env $ \unlift ->
            P.runCmd backend idx $ unlift . cmd

-- | Reinterpret a child 'Aggregate' through a parent 'Aggregate'.
--
-- The child index is used only to choose the parent persistence index. That
-- mapped parent index is consequently also the parent transaction and storage
-- boundary, and is the lock boundary for backends such as Postgres that lock
-- aggregate updates. The model projection and event injection describe one
-- fixed view within every mapped parent aggregate.
--
-- If two child indexes map to the same parent index, they intentionally alias
-- the same child view and parent transaction/storage key. With a locking
-- backend, they also serialize on that key.
-- Such collisions should be deliberate; changing @mapIndex@ for existing data
-- is a persistence-routing migration.
-- A child transaction is forwarded as exactly one parent transaction; separate
-- child 'runTransactionI' calls remain separate committed parent transactions.
-- If the child callback fails before returning, no parent events are submitted
-- and the parent interpreter's normal rollback semantics apply.
--
-- The mapping functions must be total. In particular, the updated-result
-- projection is evaluated after the backend commits, following the normal
-- 'runTransactionI' return convention. Do not start a nested transaction for
-- the same parent aggregate from the child transaction callback.
runSubAggregateI
    :: forall sub parent es a
     . Aggregate parent :> es
    => (DomainIndex sub -> DomainIndex parent)
    -- ^ Route a child index to the parent persistence and transaction index.
    -> (DomainModel parent -> DomainModel sub)
    -- ^ Project the fixed child view from a parent model.
    -> (DomainEvent sub -> DomainEvent parent)
    -- ^ Inject a child event into the parent event type.
    -> Eff (Aggregate sub : es) a
    -> Eff es a
runSubAggregateI mapIndex projectModel injectEvent = interpret $ \env -> \case
    RunTransactionI childIndex childTransaction ->
        localSeqUnlift env $ \unlift ->
            runTransactionI @parent (mapIndex childIndex) $ \parentModel -> do
                let childModel = projectModel parentModel
                (mkResult, childEvents) <-
                    unlift (childTransaction childModel)
                pure
                    ( mkResult . projectModel
                    , fmap injectEvent childEvents
                    )

-- | Reinterpret a child 'Projection' through a parent 'Projection'.
--
-- Model reads are routed through the mapped parent index and then projected.
-- Event-list reads fetch that parent history and retain the events decoded by
-- @projectEvent@. 'P.Stored' is traversed rather than rebuilt, so UUIDs,
-- timestamps, and event ordering are preserved exactly.
--
-- A decoder may retain a wider parent event language even when the child model
-- is narrower. Returning 'Nothing' declares that a parent event is irrelevant
-- to this child history; such an event must also leave the projected child model
-- unchanged.
-- All mapping functions must be total.
runSubProjectionI
    :: forall sub parent es a
     . Projection parent :> es
    => (DomainIndex sub -> DomainIndex parent)
    -- ^ Route a child index to the parent persistence index.
    -> (DomainModel parent -> DomainModel sub)
    -- ^ Project the fixed child view from a parent model.
    -> (DomainEvent parent -> Maybe (DomainEvent sub))
    -- ^ Decode zero or one child event from each parent event.
    -> Eff (Projection sub : es) a
    -> Eff es a
runSubProjectionI mapIndex projectModel projectEvent = interpret $ \_ -> \case
    GetModelI childIndex ->
        projectModel <$> getModelI @parent (mapIndex childIndex)
    GetEventListI childIndex ->
        mapMaybe (traverse projectEvent)
            <$> getEventListI @parent (mapIndex childIndex)

-- | Reinterpret adjacent child 'Aggregate' and 'Projection' effects through
-- their parent effects.
--
-- Typical use for a model-only zoom that retains the parent's event language:
--
-- @
-- runSubDomainI @BillingDomain @ClaimDomain
--     billingClaimToClaim
--     claimBilling
--     id
--     Just
-- @
--
-- Application mappings must obey both of these laws:
--
-- * @projectEvent (injectEvent event) == Just event@.
-- * Applying parent events and then projecting agrees with decoding those
--   events and applying the retained child events. A parent event decoded as
--   'Nothing' must leave the projected child model unchanged.
--
-- These laws apply to all parent events, including deliberately broad events,
-- not only events created through @injectEvent@.
runSubDomainI
    :: forall sub parent es a
     . ( Aggregate parent :> es
       , Projection parent :> es
       )
    => (DomainIndex sub -> DomainIndex parent)
    -> (DomainModel parent -> DomainModel sub)
    -> (DomainEvent sub -> DomainEvent parent)
    -> (DomainEvent parent -> Maybe (DomainEvent sub))
    -> Eff (Aggregate sub : Projection sub : es) a
    -> Eff es a
runSubDomainI mapIndex projectModel injectEvent projectEvent =
    runSubProjectionI @sub @parent mapIndex projectModel projectEvent
        . runSubAggregateI @sub @parent mapIndex projectModel injectEvent

-- | @NoIndex@ specialization of 'runSubAggregateI'.
runSubAggregate
    :: forall sub parent es a
     . ( DomainIndex sub ~ NoIndex
       , DomainIndex parent ~ NoIndex
       , Aggregate parent :> es
       )
    => (DomainModel parent -> DomainModel sub)
    -> (DomainEvent sub -> DomainEvent parent)
    -> Eff (Aggregate sub : es) a
    -> Eff es a
runSubAggregate projectModel injectEvent =
    runSubAggregateI @sub @parent
        (const NoIndex)
        projectModel
        injectEvent

-- | @NoIndex@ specialization of 'runSubProjectionI'.
runSubProjection
    :: forall sub parent es a
     . ( DomainIndex sub ~ NoIndex
       , DomainIndex parent ~ NoIndex
       , Projection parent :> es
       )
    => (DomainModel parent -> DomainModel sub)
    -> (DomainEvent parent -> Maybe (DomainEvent sub))
    -> Eff (Projection sub : es) a
    -> Eff es a
runSubProjection projectModel projectEvent =
    runSubProjectionI @sub @parent
        (const NoIndex)
        projectModel
        projectEvent

-- | @NoIndex@ specialization of 'runSubDomainI'.
runSubDomain
    :: forall sub parent es a
     . ( DomainIndex sub ~ NoIndex
       , DomainIndex parent ~ NoIndex
       , Aggregate parent :> es
       , Projection parent :> es
       )
    => (DomainModel parent -> DomainModel sub)
    -> (DomainEvent sub -> DomainEvent parent)
    -> (DomainEvent parent -> Maybe (DomainEvent sub))
    -> Eff (Aggregate sub : Projection sub : es) a
    -> Eff es a
runSubDomain projectModel injectEvent projectEvent =
    runSubDomainI @sub @parent
        (const NoIndex)
        projectModel
        injectEvent
        projectEvent
