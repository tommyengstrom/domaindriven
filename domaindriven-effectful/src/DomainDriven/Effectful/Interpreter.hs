module DomainDriven.Effectful.Interpreter
    ( runAggregate
    , runProjection
    ) where

import DomainDriven.Effectful.Aggregate
import DomainDriven.Effectful.Domain
import DomainDriven.Effectful.Projection
import DomainDriven.Persistance.Class (ReadModel, WriteModel)
import DomainDriven.Persistance.Class qualified as P
import Effectful
import Effectful.Dispatch.Dynamic
import Prelude

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
