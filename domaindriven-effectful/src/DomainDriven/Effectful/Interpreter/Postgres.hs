{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module DomainDriven.Effectful.Interpreter.Postgres where

import Data.Aeson
import DomainDriven.Effectful.Aggregate
import DomainDriven.Effectful.Domain
import DomainDriven.Effectful.Projection
import DomainDriven.Persistance.Class (WriteModel)
import DomainDriven.Persistance.Class qualified as P
import DomainDriven.Persistance.Postgres.Internal
import DomainDriven.Persistance.Postgres.Types
import Effectful
import Effectful.Dispatch.Dynamic
import Prelude

-- | Run the Projection' effect using an in-memory backend (new domain API)
runProjectionPostgres
    :: forall domain es a index
     . ( IOE :> es
       , FromJSON (DomainEvent domain)
       , IsPgIndex index
       , index ~ (DomainIndex domain)
       , P.Index (PostgresEvent (DomainModel domain) (DomainIndex domain) (DomainEvent domain))
            ~ index
       )
    => PostgresEvent (DomainModel domain) (DomainIndex domain) (DomainEvent domain)
    -> Eff (Projection domain : es) a
    -> Eff es a
runProjectionPostgres backend = interpret $ \_ -> \case
    GetModelI idx -> liftIO $ P.getModel backend idx
    GetEventListI idx -> liftIO $ P.getEventList backend idx

-- | Run the Aggregate effect using an in-memory backend (new domain API)
runAggregateInMemory
    :: forall domain es a index
     . ( IOE :> es
       , index ~ (DomainIndex domain)
       , P.Index (PostgresEvent (DomainModel domain) (DomainIndex domain) (DomainEvent domain))
            ~ index
       , WriteModel (PostgresEvent (DomainModel domain) (DomainIndex domain) (DomainEvent domain))
       )
    => PostgresEvent (DomainModel domain) (DomainIndex domain) (DomainEvent domain)
    -> Eff (Aggregate domain : es) a
    -> Eff es a
runAggregateInMemory backend = interpret $ \env -> \case
    RunTransactionI idx cmd -> do
        localSeqUnlift env $ \unlift ->
            P.runCmd backend idx $ unlift cmd
