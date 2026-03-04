# Project Setup: 3-Package Event Migration Pattern

Split your project into three packages for safe, incremental event schema evolution with compile-time guarantees.

## Package Structure

For event design principles (small events, hierarchical types) see [event-design.md](event-design.md).

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
    └── src/MyProject/Runner.hs   # Chains migrations, verifies consistency
```

### `<project>-events`
Canonical, current event types. This is the only package you edit when changing events. Events live in a separate package so the migration package can import frozen snapshots at each version — without the split, you can't have two versions of the same module in scope at once.

### `<project>-migrations`
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
        $ MigrateUsing migrationV50
        $ MigrateUsing migrationV49
        $ MigrateUsing discardedMigration
        $ MigrateUsing discardedMigration
        -- ... older discarded versions ...
        $ InitialVersion "events"
```

Each `MigrateUsing` wraps one migration step. The chain reads newest-first, oldest-last, with `InitialVersion` at the bottom.

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

## `discardedMigration`

Once all database instances have migrated past a version, replace its migration with `discardedMigration` to improve compile times:

```haskell
-- | A migration that is no longer kept.
-- Once all instances have migrated past this version, the migration code
-- can be discarded. This improves compile speed.
discardedMigration :: PreviousEventTableName -> EventTableName -> Connection -> IO ()
discardedMigration _ etName conn = void $ createEventTable' conn etName
```

You can also remove the corresponding `EventN.*` snapshot modules from the migrations package.

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
4. **Chain**: add `MigrateUsing migrationVN $` to the top of the chain in Runner.hs
5. **Update Latest**: change the `Latest` import to `EventN`
6. **Compile**: `ensureMigrationIsUpToDate` verifies everything is consistent
7. **Over time**: replace old migrations with `discardedMigration` and remove their snapshots
