{-# OPTIONS_GHC -Wno-orphans #-}
module EventMigration (eventTable) where

import Data.ShapeCoerce
import Database.PostgreSQL.Simple (Connection)
import DomainDriven.Persistance.Class
import DomainDriven.Persistance.Postgres
import DomainDriven.Persistance.Postgres.Migration
import Event.V1 qualified as V1
import Event.V2 qualified as V2
import Prelude

fixEvent :: Stored V1.CounterEvent -> Stored V2.CounterEvent
fixEvent = shapeCoerce

-- Automatic ShapeCoercible fails because the constructor names changed:
--
--     • Cannot shapeCoerce between types:
--         From: V1.CounterEvent
--         To: V2.CounterEvent
--
--       Reason: Constructor name mismatch
--         'CounterIncreased ≠ 'CounterIncreasedBy
--
--       Solution: Write instance `ShapeCoercible V1.CounterEvent V2.CounterEvent`

instance ShapeCoercible V1.CounterEvent V2.CounterEvent where
    shapeCoerce = \case
        V1.CounterIncreased -> V2.CounterIncreasedBy 1
        V1.CounterDecreased -> V2.CounterDecreasedBy 1

migrate ::
    PreviousEventTableName ->
    EventTableName ->
    Connection ->
    IO ()
migrate prevEtName etName conn = do
    migrate1to1 @NoIndex
        conn
        prevEtName
        etName
        fixEvent

eventTable :: EventTable
eventTable =
    MigrateUsing migrate
        $ InitialVersion "counter_events"
