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
        ├── Server.hs             # Handlers, Effects alias, withX helpers
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
        $ MigrateWith "v50-split-name" migrationV50
        $ MigrateWith "v49-add-email" migrationV49
        $ TableName "events" 48
```

Each `MigrateWith` wraps one migration step and carries a **tag**: a non-empty string that is unique within the chain and identifies the migration in the `domaindriven_migrations` metadata table. The chain reads newest-first, oldest-last, with `TableName` at the bottom naming the **oldest version this code still knows about** (not necessarily 1). The current table is `<base>_v<TableName version + number of MigrateWith wrappers>` — `events_v50` above.

At startup `postgresWriteModel` verifies the chain against the database before anything runs:
- a fresh database gets only the current table (`events_v50`) plus metadata rows for the whole chain — the migration functions do **not** run
- an existing database is migrated forward one step at a time, recording each step's tag
- tags in code that disagree with the recorded tags, a database ahead of the code, or a database below the `TableName` version fail fast with a `MigrationError` whose message says what to do (an off-by-one `TableName` version is diagnosed explicitly)

Because verification is by tag, never rename a tag once it has been deployed (or update `domaindriven_migrations` by hand at the same time).

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

Once every database instance has migrated past a version, delete its migration from code to improve compile times: remove the `MigrateWith` wrapper and bump the `TableName` version by one.

```haskell
-- before: events_v50 = TableName 48 + 2 wrappers
MigrateWith "v50-split-name" migrationV50 $ MigrateWith "v49-add-email" migrationV49 $ TableName "events" 48
-- after:  events_v50 = TableName 49 + 1 wrapper
MigrateWith "v50-split-name" migrationV50 $ TableName "events" 49
```

The current table name does not change, and startup keeps verifying the remaining tags against the recorded history. A database that is still at a deleted version (here: live at `events_v48`) refuses to start with `DatabaseBehindCodeBase` instead of silently coming up empty — deploy a build that still contains the missing migrations first.

You can also remove the corresponding `EventN.*` snapshot modules from the migrations package. There is no need for placeholder migrations: a `MigrateWith` that does nothing would run against real data if a database were still at that version.

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
4. **Chain**: add `MigrateWith "<unique-tag>" migrationVN $` to the top of the chain in Runner.hs
5. **Update Latest**: change the `Latest` import to `EventN`
6. **Compile**: `ensureMigrationIsUpToDate` verifies everything is consistent
7. **Over time**: delete old migrations (remove the `MigrateWith`, bump the `TableName` version) and remove their snapshots
