---
name: domaindriven
description: Help users build applications with the domaindriven Haskell event sourcing library
---

You are an expert in the `domaindriven` Haskell library. Follow these patterns exactly.

## Architecture

Synchronous event sourcing with Effectful effects and Servant APIs.

Dependencies: `domaindriven-core` (persistence backends), `domaindriven` (Effectful API layer), `shape-coerce` (event migration).

## Core Pattern

```haskell
-- 1. Domain triple
type MyDomain = Domain MyModel MyEvent NoIndex

-- 2. Events (must derive Generic, ToJSON, FromJSON)
data MyEvent = ThingHappened | ValueSet Int

-- 3. Apply events to build state
applyEvent :: MyModel -> Stored MyEvent -> MyModel
applyEvent m (Stored evt _ _) = case evt of
    ThingHappened -> m { flag = True }
    ValueSet n    -> m { value = n }

-- 4. Create backend
-- Testing:
backend <- createForgetful applyEvent initialModel
-- Production:
pool <- simplePool' connectInfo
backend <- postgresWriteModel pool eventTable applyEvent initialModel

-- 5. Handlers use Aggregate (write) and Projection (read) effects
myHandler
    :: (Aggregate MyDomain :> es, Projection MyDomain :> es, Error ServerError :> es)
    => MyAPI (AsServerT (Eff es))
myHandler = MyAPI
    { getState = getModel        -- Projection: read current state
    , doThing  = runTransaction \model ->
        pure (const (), [ThingHappened])  -- (extractor from updated model, events)
    }

-- 6. Wire effect stack
runEffects :: Eff '[Projection MyDomain, Aggregate MyDomain, Error ServerError, IOE] a -> Handler a
runEffects m = do
    a <- liftIO . runEff . runErrorNoCallStack @ServerError
        . runAggregate backend . runProjection backend $ m
    either throwError pure a
```

## Key Types

```haskell
data Stored a = Stored
    { storedEvent     :: a
    , storedTimestamp :: UTCTime
    , storedUUID      :: UUID
    }

data Domain (model :: Type) (event :: Type) (index :: Type) = Domain

data NoIndex = NoIndex
newtype Indexed = Indexed Text

data Aggregate (domain :: Type) :: Effect where
    RunTransactionI
        :: DomainIndex domain
        -> (DomainModel domain -> Eff es (DomainModel domain -> a, [DomainEvent domain]))
        -> Aggregate domain (Eff es) a

data Projection domain :: Effect where
    GetModelI    :: DomainIndex domain -> Projection domain m (DomainModel domain)
    GetEventListI :: DomainIndex domain -> Projection domain m [Stored (DomainEvent domain)]
```

## `runTransaction` Return Convention

The callback returns `(Model -> a, [Event])`:
- First element extracts return value from the *updated* model (after events applied)
- `const ()` when nothing to return, `id` to return whole model, `(.someField)` for a field

## Indexed Aggregates

Use `runTransactionI` and `getModelI` with an index:

```haskell
type MyDomain = Domain MyModel MyEvent Indexed
increase :: Indexed -> Eff es Int
increase idx = runTransactionI idx \model -> pure ((.counter), [Increased])
```

## Servant Integration

`FieldNameAsPath` derives URL paths from record field names. Each field name becomes a path segment automatically:

```haskell
data CounterAPI mode = CounterAPI
    { get      :: mode :- Get '[JSON] Int       -- GET  /get
    , increase :: mode :- Post '[JSON] Int      -- POST /increase
    , decrease :: mode :- Post '[JSON] Int      -- POST /decrease
    } deriving Generic

instance ApiTagFromLabel CounterAPI
```

Compare with standard Servant Generic where you must spell out paths in the type:

```haskell
data CounterAPI mode = CounterAPI
    { get      :: mode :- Get '[JSON] Int                       -- GET  /
    , increase :: mode :- "increase" :> Post '[JSON] Int        -- POST /increase
    , decrease :: mode :- "decrease" :> Post '[JSON] Int        -- POST /decrease
    } deriving Generic
```

Serve with:

```haskell
serve (Proxy @(FieldNameAsPathApi CounterAPI))
    $ hoistServer (Proxy @(FieldNameAsPathApi CounterAPI)) runEffects
    $ FieldNameAsPathServer counterHandler
```

## Event Migration (Postgres)

Use `shape-coerce` for migrations. Let the compiler guide you:

1. First, try just `shapeCoerce`. If the old and new event types are structurally identical (same constructor names, same fields), it works automatically via Generics.

2. If the types differ, the compiler will tell you exactly what doesn't match (constructor name mismatch, field mismatch, etc.). Write a manual `ShapeCoercible` instance for the cases it can't derive:

```haskell
-- V1
data CounterEvent = CounterIncreased | CounterDecreased

-- V2: constructors changed, so automatic shapeCoerce fails at compile time.
-- Write the instance the compiler is asking for:
data CounterEvent = CounterIncreasedBy Int | CounterDecreasedBy Int

instance ShapeCoercible V1.CounterEvent V2.CounterEvent where
    shapeCoerce = \case
        V1.CounterIncreased -> V2.CounterIncreasedBy 1
        V1.CounterDecreased -> V2.CounterDecreasedBy 1
```

3. Wire it into the migration. `Stored` is a `Functor` so `shapeCoerce` on `Stored a -> Stored b` works automatically once the inner event type has an instance:

```haskell
eventTable :: EventTable
eventTable = MigrateUsing myMigration $ InitialVersion "my_events"
-- Creates my_events_v1 (initial), my_events_v2 (after migration)

myMigration :: EventMigration
myMigration prev next conn = migrate1to1 @NoIndex conn prev next shapeCoerce
```

For multi-package project setup with compile-time migration safety, see [project-setup.md](project-setup.md).

## Imports

```haskell
import DomainDriven  -- re-exports everything from the effectful layer
-- Or individually:
import DomainDriven.Persistance.Class          -- Stored, ReadModel, WriteModel, NoIndex, Indexed
import DomainDriven.Persistance.ForgetfulInMemory -- createForgetful
import DomainDriven.Persistance.Postgres       -- PostgresEvent, postgresWriteModel, simplePool
import DomainDriven.Persistance.Postgres.Migration -- migrate1to1, migrate1toMany
import Data.ShapeCoerce                        -- shapeCoerce, ShapeCoercible
```
