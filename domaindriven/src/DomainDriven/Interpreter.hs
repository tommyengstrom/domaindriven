module DomainDriven.Interpreter
    ( runAggregate
    , runBeamAggregate
    , runBeamProjection
    , runProjection
    ) where

import DomainDriven.Aggregate
import DomainDriven.BeamAggregate
import DomainDriven.BeamProjection
import Data.Aeson (ToJSON)
import Database.Beam.Postgres
    ( Pg
    , Postgres
    )
import Database.Beam.Schema.Tables (Database)
import DomainDriven.Persistance.Postgres (IsPgIndex)
import DomainDriven.Domain
import DomainDriven.Projection
import DomainDriven.Persistance.Class (ReadModel, WriteModel)
import DomainDriven.Persistance.Class qualified as P
import DomainDriven.Persistance.Postgres.Beam (PostgresBeam, runBeamCmd)
import DomainDriven.Persistance.Postgres.Beam qualified as PB
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Internal.Monad (unsafeEff_)
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

runBeamProjectionWith
    :: (forall x. Pg x -> IO x)
    -> Eff (BeamProjection db : es) a
    -> Eff es a
runBeamProjectionWith runPgIO =
    interpret $ \_ -> \case
        RunPg query -> unsafeEff_ $ runPgIO query

-- | Run the Beam-backed relational projection effect.
runBeamProjection
    :: forall index db model event es a
     . PostgresBeam index db model event
    -> Eff (BeamProjection db : es) a
    -> Eff es a
runBeamProjection backend =
    runBeamProjectionWith (PB.runBeamPg backend)

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

-- | Run the Beam-specific transactional aggregate effect.
runBeamAggregate
    :: forall domain db es a
     . ( IOE :> es
       , Database Postgres db
       , IsPgIndex (DomainIndex domain)
       , ToJSON (DomainEvent domain)
       )
    => PostgresBeam (DomainIndex domain) db (DomainModel domain) (DomainEvent domain)
    -> Eff (BeamAggregate domain db : es) a
    -> Eff es a
runBeamAggregate backend = interpret $ \env -> \case
    RunBeamTransactionI idx cmd ->
        localSeqUnliftIO env $ \unliftIO ->
            liftIO $
                runBeamCmd backend idx $ \runPgIO ->
                    unliftIO $ runBeamProjectionWith runPgIO cmd
