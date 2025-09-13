{-# options_GHC -Wno-orphans #-}
module EventMigration where

import Prelude
import Data.ShapeCoerce
import DomainDriven.Persistance.Class
import DomainDriven.Persistance.Postgres
import Database.PostgreSQL.Simple (Connection)
import DomainDriven.Persistance.Postgres.Migration
import Event.V1 qualified as V1
import Event.V2 qualified as V2



fixEvent :: Stored V1.Event -> Stored V2.Event
fixEvent = shapeCoerce

-- /home/tommy/git/domaindriven/domaindriven-effectful-examples/postgres/EventMigra
-- tion.hs:16:12: error: [GHC-64725]
--     • Cannot shapeCoerce between types:
--         From: V1.UserEvent
--         To: V2.UserEvent
--
--       Reason: Left side has a single constructor but right side is a sum type
--       Left constructor: "UserNameChanged"
--       Right side: Multiple constructors (sum type)
--
--       Solution: Write instance `ShapeCoercible V1.UserEvent V2.UserEvent`
--     • In the expression: shapeCoerce
--       In an equation for ‘fixEvent’: fixEvent = shapeCoerce
--    |
-- 16 | fixEvent = shapeCoerce
--    |            ^^^^^^^^^^^
-- -- | V2.UserEvent has a new constructor

instance ShapeCoercible V1.UserEvent V2.UserEvent where
  shapeCoerce = \case
    V1.UserCreated name -> V2.UserCreated name
    V1.UserNameChanged name -> V2.UserNameChanged name

migrate ::
    PreviousEventTableName ->
    EventTableName ->
    Connection ->
    IO ()
migrate prevEtname etName conn = do
    migrate1to1 @NoIndex
        conn
        prevEtname
        etName
        fixEvent

eventTable :: EventTable
eventTable = MigrateUsing migrate
    $ InitialVersion "my_events"



