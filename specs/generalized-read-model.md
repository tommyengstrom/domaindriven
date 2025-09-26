# Generalized `ReadModel`

This iteration incorporates two refinements:

1. No defaults inside the class. Defining a `ReadModel` is rare and explicit implementations keep behaviour obvious.
2. The `Model p` associated type is removed. Backends now commit only to their runtime `ResolvableModel` and query surface.

## Supporting types

```haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

newtype StateQuery state r = StateQuery
    { runStateQuery :: state -> r
    }

identityQuery :: StateQuery s s
identityQuery = StateQuery id

data BeamQuery be db r where
    BeamQuery :: (forall m. MonadBeam be m => db -> m r) -> BeamQuery be db r
```

The helper newtypes are optional; each backend chooses its own `Query p` type alias.

## Proposed class

```haskell
class ReadModel p where
    type Event p      :: Type -- The event type
    type Index p      :: Type -- Index. can be unit if not applicable.
    type ResolvableModel p :: Type -- Could be the model itself, or some way of fetching it in `RunnerMonad p`
    type RunnerMonad p :: Type -> Type -- The Monad where `applyEvent` and `queryModel` runs. 
    type Query p      :: Type -> Type -- The types of queries that can be run. Identity for in memory models.

    applyEvent
        :: p
        -> Index p
        -> Stored (Event p)
        -> ResolvableModel p
        -> RunnerMonad p (ResolvableModel p)

    queryModel
        :: p -> Index p -> Query p r -> RunnerMonad p r

    getEventList   :: p -> Index p -> IO [Stored (Event p)]
    getEventStream :: HasCallStack => p -> Index p -> Stream IO (Stored (Event p))
```

Every backend must explicitly choose its `RunnerMonad`, `ResolvableModel`, and `Query` carrier. There are no defaults.

## In-memory instance

```haskell
instance Hashable index => ReadModel (ForgetfulInMemory model index event) where
    type Event (ForgetfulInMemory model index event)              = event
    type Index (ForgetfulInMemory model index event)              = index
    type ResolvableModel (ForgetfulInMemory model index event)    = model
    type RunnerMonad (ForgetfulInMemory model index event)        = IO
    type Query (ForgetfulInMemory model index event)              = StateQuery model

    -- transactionalUpdate in the backend reads the current state from stateRef
    -- and supplies it here as `model`.
    applyEvent ff _ stored model =
        pure (apply ff model stored)

    queryModel ff idx (StateQuery k) = do
        state <- HM.lookupDefault (seed ff) idx <$> readIORef (stateRef ff)
        pure (k state)

    getEventList ff idx =
        HM.lookupDefault [] idx <$> readIORef (events ff)

    getEventStream ff idx =
        Stream.bracketIO
            (HM.lookupDefault [] idx <$> readIORef (events ff))
            (const (pure ()))
            Stream.fromList
```

`StateQuery` lets callers recover the old behaviour (`identityQuery` yields the whole model) or project fields without additional boilerplate.

## Beam/Postgres instance

```haskell
data BeamReadModel = BeamReadModel
    { pool :: Pool Connection
    , db   :: DatabaseSettings Postgres UserDb
    }

instance HasEventProjection UserEvent => ReadModel BeamReadModel where
    type Event BeamReadModel           = UserEvent
    type Index BeamReadModel           = UUID
    type ResolvableModel BeamReadModel = BeamHandle Postgres Connection UserDb
    type RunnerMonad BeamReadModel     = BeamTx Postgres Connection
    type Query BeamReadModel           = BeamQuery Postgres UserDb

    applyEvent _ userId stored handle =
        runBeamUpdate handle (mkEventProjectionQuery userId stored)

    queryModel BeamReadModel{pool, db} _ (BeamQuery q) =
        withResource pool $ \conn -> runBeamPostgres conn (q db)

    getEventList   = ...
    getEventStream = ...
```

No materialised Haskell model is created unless a query requests it. Users express read patterns by constructing `BeamQuery` values:

```haskell
latestBalance :: UUID -> BeamQuery Postgres UserDb (Maybe Balance)
latestBalance accountId = BeamQuery $ \db ->
    runSelectReturningOne $ select $ do
        row <- all_ (dbAccountBalance db)
        guard_ (row.accountId ==. val_ accountId)
        pure row.balance
```

## Pros

* Backends are explicit about every capability (`RunnerMonad`, `ResolvableModel`, `Query`), making behaviour easy to audit.
* Pure backends still feel lightweight—`StateQuery` gives them the same ergonomics as before.
* Persistent backends expose typed query interfaces instead of serialising huge projections.
* The core library no longer needs to know anything about backend-specific model types.

## Cons

* Implementations must fill in all hooks; no defaults means more code per backend, though the count of backends is small.
* Callers must evaluate the chosen `RunnerMonad` (`Either`, `BeamTx`, etc.), which may need shims at call sites.
* Additional associated types surface in type errors.
* Each backend has to design a query language (`StateQuery`, `BeamQuery`) and maintain it as part of its API.
