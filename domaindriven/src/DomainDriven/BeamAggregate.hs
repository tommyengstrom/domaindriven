{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module DomainDriven.BeamAggregate where

import Data.Type.Equality
import Database.Beam.Postgres (Pg)
import DomainDriven.BeamProjection
import DomainDriven.Domain
import DomainDriven.Persistance.Class (NoIndex (..))
import Effectful
import Effectful.TH

data BeamAggregate domain db :: Effect where
    RunBeamTransactionI
        :: DomainIndex domain
        -> Eff (BeamProjection db : es) (Pg a, [DomainEvent domain])
        -> BeamAggregate domain db (Eff es) a

type instance DispatchOf (BeamAggregate domain db) = 'Dynamic

$(makeEffect ''BeamAggregate)

runBeamTransaction
    :: forall domain db es a
     . BeamAggregate domain db :> es
    => DomainIndex domain ~ NoIndex
    => Eff (BeamProjection db : es) (Pg a, [DomainEvent domain])
    -> Eff es a
runBeamTransaction = runBeamTransactionI NoIndex
