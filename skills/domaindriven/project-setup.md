# Project Setup: 3-Package Event Migration Pattern

Split your project into three packages for safe, incremental event schema evolution with compile-time guarantees.

## Package Structure

For event design principles (small events, hierarchical types) see [event-design.md](event-design.md).
For handler patterns (withX, lookups, setField) see [handler-patterns.md](handler-patterns.md).
For application wiring (effects, testing, config) see [app-wiring.md](app-wiring.md).

```
my-project/
├── lib/my-project-events/        # Current event types
│   └── src/MyProject/Event.hs
├── lib/my-project-migrations/    # Versioned snapshots + migration logic
│   └── src/
│       ├── Event49/Event.hs      # Snapshot of events at version 49
│       ├── Event50/Event.hs      # Snapshot of events at version 50
│       ├── Migration/V50.hs      # Migration from 49 → 50
│       └── ...
└── services/my-project/          # Main service
    └── src/MyProject/
        ├── Types.hs              # ID newtypes, enumerations, entity records
        ├── Event.hs              # Hierarchical event types (or import from events pkg)
        ├── Model.hs              # Domain model, emptyModel, Domain type alias
        ├── EventHandler.hs       # applyEvent with optics-based dispatch
        ├── Command.hs            # Request body types (one per mutation endpoint)
        ├── Api.hs                # Servant API types with FieldNameAsPath
        ├── Api/                  # Split large APIs into sub-modules
        ├── Server.hs             # Handlers, withX helpers
        ├── Hooks/                # Effectful hooks, one per file
        │   └── OnUserCreated.hs
        └── Main.hs               # Entry point, backend creation, effect stack wiring
```

### `<project>-events`
Canonical, current event types. This is the only package you edit when changing events. Events live in a separate package so the migration package can import frozen snapshots at each version — without the split, you can't have two versions of the same module in scope at once.

**Must not depend on the main service package.** Keep dependencies minimal — typically just `aeson`, `deepseq`, `text`, `time`, and similar leaf libraries. Every event type must derive `NFData`, so the package that defines current events needs a direct `deepseq` dependency. The events package defines pure data types; it should not pull in Servant, Effectful, database libraries, or anything heavy.

**All types referenced by event field definitions live here too** — domain primitives, value objects, enums, newtypes. If an event field uses `Email` or `PhoneNumber`, those types belong in `<project>-events`, not in the main service package. Otherwise handlers end up wrapping/unwrapping shims to convert between "the service's `Email`" and "the event's `Email`", and migrations get harder because the frozen snapshots can't see the current domain types.

### `<project>-migrations`
**Must not depend on the main service package.** Dependencies should be limited to `<project>-events`, `domaindriven-core`, `shape-coerce`, `deepseq`, and basic libraries. Frozen event snapshots also derive `NFData`, so the migrations package needs `deepseq` when it owns those snapshot modules. Keeping this package lightweight ensures fast compilation of migration logic.

Two kinds of modules:
- **Event snapshots** (`EventN.*`): Frozen copies of `<project>-events` at version N. Created by copying all modules from `<project>-events` into an `EventN.*` namespace.
- **Migration modules** (`Migration.VN`): Convert `Event(N-1)` → `EventN` using `shapeCoerce`.

### `<project>` (main service)
Contains `Runner.hs` that chains all migrations and uses `ensureMigrationIsUpToDate` to verify the latest snapshot matches current events.

## Creating a New Event Snapshot

When you need to migrate (version N-1 → N):

1. Copy all modules from `<project>-events/src/` into `<project>-migrations/src/EventN/`
2. Rename the module declarations (e.g. `MyProject.Event.Types` → `EventN.Event.Types`)
3. Update internal imports within the snapshot to use `EventN.*`
4. Add the new `EventN.*` modules to `<project>-migrations.cabal`

## Writing a Migration Module

```haskell
module Migration.VN where

import EventPrev.Event qualified as Old   -- previous snapshot
import EventN.Event    qualified as New   -- new snapshot
import Data.ShapeCoerce

fixEvent :: ShapeCoercible (Old.MyEvent) (New.MyEvent)
         => Stored (Old.MyEvent) -> Stored (New.MyEvent)
fixEvent = fmap shapeCoerce

-- If types changed structurally, write manual instances:
instance ShapeCoercible Old.SomeType New.SomeType where
    shapeCoerce old = New.SomeType
        { field1 = shapeCoerce (Old.field1 old)
        , newField = defaultValue  -- added field
        }

myMigration :: PreviousEventTableName -> EventTableName -> Connection -> IO ()
myMigration prev next conn = migrate1to1 @NoIndex conn prev next fixEvent
```

The compiler guides you: try `shapeCoerce` first. If old and new types are structurally identical, it works automatically. If not, the compiler error tells you exactly which types differ and need a manual `ShapeCoercible` instance.

## Chaining Migrations in Runner.hs

```haskell
eventTable :: EventTable
eventTable =
    ensureMigrationIsUpToDate
        $ MigrateTo 50 migrationV50
        $ MigrateTo 49 migrationV49
        $ TableName "events" 48
```

Each `MigrateTo` wraps one migration step and states the **version it produces**: `MigrateTo 50` reads `events_v49` and writes `events_v50`. The chain reads newest-first, oldest-last, with `TableName` at the bottom naming the **oldest version this code still knows about** (not necessarily 1). Steps must be numbered consecutively from one above the `TableName` version; the current table is the one the top step produces — `events_v50` above.

At startup `postgresWriteModel` checks the chain and then looks at which `events_vN` tables exist:
- a database without any gets only the current table (`events_v50`) — the migration functions do **not** run
- otherwise the highest existing table is the current one, provided the previous existing table has an enabled retirement trigger; the steps above it run one at a time
- a misnumbered chain, a table name over 63 characters, a database ahead of the code, or a database below the `TableName` version fail fast with a `MigrationError` whose message says what to do

The chain check is pure, so a test can run it without a database: `validateEventTable eventTable`.

The version numbers are the only link between the code and the database. A chain numbered one too high (`MigrateTo 51 migrationV50 $ TableName "events" 50` while the live table is `events_v50`) passes the check and runs `migrationV50` again, on the live table. Before deploying a renumbered chain, check that `getEventTableName eventTable` is the name of the live table.

### Migration functions must not end the transaction

The whole chain runs in one startup transaction that also holds the locks on the previous table. A migration function that calls `commit`, `rollback` or `withTransaction` on the connection it is given ends that transaction and releases the locks mid-chain. Startup detects this and fails with `MigrationEndedTransaction`; the previous table stays live. If the function committed, the new table exists with whatever was copied before the commit — drop it before starting again. Only use the connection for plain statements and the `migrate*` helpers.

Retries and concurrent starters check that the previous existing table is retired before accepting the newest version. If it is still writable, startup fails with `IncompleteMigration`, including when the migration chain has been trimmed. This check examines only the previous existing version, not the full history.

## `ensureMigrationIsUpToDate`

A zero-cost identity function that provides compile-time verification:

```haskell
ensureMigrationIsUpToDate
    :: ShapeIsomorphic MyEvent Latest.MyEvent
    => x -> x
ensureMigrationIsUpToDate = id
```

`ShapeIsomorphic a b` means `(ShapeCoercible a b, ShapeCoercible b a)` — the types must be structurally identical in both directions. This ensures:
- If you change events in `<project>-events` without creating a new snapshot, **compilation fails**
- If you create a snapshot but forget to update the `Latest` import in Runner.hs, **compilation fails**

The `Latest` import aliases the newest snapshot:

```haskell
import EventN.Event qualified as Latest
```

## Deleting Old Migrations

Once every database instance has migrated past a version, delete its migration from code to improve compile times: remove the `MigrateTo` wrapper and raise the `TableName` version to one below the oldest remaining step.

```haskell
-- before
MigrateTo 50 migrationV50 $ MigrateTo 49 migrationV49 $ TableName "events" 48
-- after
MigrateTo 50 migrationV50 $ TableName "events" 49
```

The current table name does not change, and a wrong `TableName` version is rejected as a misnumbered chain. A database that is still at a deleted version (here: live at `events_v48`) refuses to start with `DatabaseBelowBaseVersion` instead of silently coming up empty — deploy a build that still contains the missing migrations first.

You can also remove the corresponding `EventN.*` snapshot modules from the migrations package. There is no need for placeholder migrations: a `MigrateTo` that does nothing would run against real data if a database were still at that version.

## Rolling Back a Migration

A build whose chain ends below the database's current table refuses to start with `DatabaseAheadOfCode`, so rolling back a deploy after a migration has run takes a manual step. With `events_v50` migrated to `events_v51`:

1. Stop every instance of the new build. Instances that were already running when the migration happened keep serving reads from the retired `events_v50` until they are restarted, so restart or stop those too.
2. `drop trigger retired on "events_v50";` — the retire trigger rejects every insert, so skipping this gives a build that starts and then fails every command with "Event table has been retired." It also blocks step 3.
3. Move any events written to `events_v51` since the migration back into `events_v50` by hand, if they must be kept.
4. `drop table "events_v51";`
5. Start the old build.

Table names are created quoted, so quote them here too if the base name has uppercase letters.

Rolling back to the previous version of the migration code and re-running the migration later is then an ordinary deploy.

## Event Snapshot Script

Automates step 1 of "Creating a New Event Snapshot". Adapt `SOURCE_PKG` and `MODULE_PREFIX` to your project:

```bash
#!/usr/bin/env bash
set -euo pipefail

SOURCE_PKG="../my-project-events"
MODULE_PREFIX="MyProject"

# Find highest existing EventN directory
LAST=$(ls -d src/Event* 2>/dev/null | grep -oP 'Event\K[0-9]+' | sort -n | tail -1)
NEXT=$(( ${LAST:-0} + 1 ))
TARGET="src/Event${NEXT}"

echo "Creating event snapshot v${NEXT} in ${TARGET}"
mkdir -p "${TARGET}"
cp -R "${SOURCE_PKG}/src/${MODULE_PREFIX}/." "${TARGET}/"
find "${TARGET}" -name '*.hs' -exec sed -i "s/${MODULE_PREFIX}\./Event${NEXT}./g" {} +
echo "Done. Remember to add Event${NEXT}.* modules to the .cabal file."
```

Run from the `<project>-migrations` directory.

## Workflow Summary

1. **Change events** in `<project>-events`
2. **Snapshot**: copy modules into `<project>-migrations` as `EventN.*`
3. **Write migration**: create `Migration.VN` importing old as `Old`, new as `New`
4. **Chain**: add `MigrateTo N migrationVN $` to the top of the chain in Runner.hs
5. **Update Latest**: change the `Latest` import to `EventN`
6. **Compile**: `ensureMigrationIsUpToDate` verifies everything is consistent
7. **Over time**: delete old migrations (remove the `MigrateTo`, raise the `TableName` version) and remove their snapshots
