# domaindriven-core

This library contains the core model and persistance handlers for domaindriven.

## Optional PostgreSQL snapshots

PostgreSQL models can opt into snapshots without changing the event log, which remains
the source of truth:

```haskell
projection <- maybe (fail "invalid projection name") pure (mkSnapshotProjectionName "account-summary")
revision <- maybe (fail "invalid projection revision") pure (mkSnapshotProjectionRevision "v1")
frequency <- maybe (fail "invalid snapshot frequency") pure (mkEveryNEvents 100)
let snapshots :: SnapshotConfig Model
    snapshots =
        SnapshotConfig
            { snapshotProjectionName = projection
            , snapshotProjectionRevision = revision
            , snapshotCodec = serialiseCborSnapshotCodec
            , snapshotFrequency = frequency
            , snapshotTimeout = defaultSnapshotTimeout
            , snapshotQueueCapacity = defaultSnapshotQueueCapacity
            , snapshotStore = postgresSnapshotStore pool
            }
bracket
    (postgresWriteModelWithSnapshots pool eventTable snapshots applyEvent initialModel)
    closeSnapshotWriter
    serveApplication
```

Here `bracket` is from `Control.Exception`; `serveApplication` uses the backend until
the application shuts down. Close the writer before destroying its connection pools.

Use `aesonJsonSnapshotCodec` for inspectable JSON snapshots, or
`serialiseCborSnapshotCodec` for typed CBOR snapshots. A CBOR model normally derives all
three required classes:

```haskell
data Model = Model
    { count :: Int
    , label :: Text
    }
    deriving stock (Generic)
    deriving anyclass (NFData, Serialise)
```

Every field type must also have the required `NFData` and `Serialise` instances. JSON
models similarly need `ToJSON`, `FromJSON`, and `NFData`.

### Projection and codec versions

Snapshots are keyed by the physical event-table version, aggregate index, projection
name, projection revision, and codec ID. Multiple projections, revisions, and encodings
can coexist over the same events. Bump the projection revision whenever the seed, fold,
model representation, or model meaning changes—even if the old snapshot still decodes.
This does not require migrating the event table. Changing event-table versions still
causes a cache miss. `getEventList` and `getEventStream` always expose the full history.

The built-in codec IDs are `aeson-json-v1` and `serialise-cbor-v1`.
`customSnapshotCodec` requires an explicit `SnapshotCodecId` as its first argument;
choose a stable ID and change it when its encoding or decoding becomes incompatible.
Identity constructors reject empty strings and NUL characters. All instances using
the same full key must agree on the seed, fold, model representation, and codec.

Create a fresh backend when changing identities or projection semantics. Record copies
share the in-memory cache and writer; changing the configuration on an existing backend
does not invalidate that cache. A shared store must be scoped to one event database/schema
so identically named event tables in unrelated databases cannot collide.

The PostgreSQL store uses one unversioned table, `public."domaindriven-snapshots"`.
Projection revisions and codec IDs are row-key fields, not table versions. Old snapshot
identities are not automatically removed, allowing mixed-version deployments and
rollbacks. Remove obsolete snapshot rows only when no deployment needs them; retain
event tables.
Coexisting projection revisions still require compatible event writers: when upgrading
from 0.6.0, coordinate the advisory-lock protocol change described in the changelog.

The existing `postgresWriteModel` and `postgresWriteModelNoMigration` constructors remain
snapshot-free and do not create a snapshot table or worker.

### Background writes and lifecycle

Due snapshots are enqueued after event commit and cache publication, including after cold
reconstruction. Encoding and storage run on a single background worker per backend, so
commands, reads, and post-update hooks do not await a snapshot write. Cancelling the
triggering request does not cancel accepted work. The worker starts lazily and exits
when idle; later traffic starts it again.

The queue defaults to 64 pending indexes, configurable with `mkSnapshotQueueCapacity`.
Different indexes run in FIFO order; repeated pending work for one index coalesces to
the newest committed candidate without changing its queue position. An active index can
also have one pending successor, which counts toward capacity. When full, new distinct
indexes are skipped and logged; existing pending entries can still be updated. Skipped
or failed work does not advance snapshot progress, and later traffic can retry. There is
no timer-based retry or backoff. The worker rechecks cadence against the latest committed
cache entry before encoding, so a queued successor may no longer need a write.

Snapshot checkpoints include the total number of relevant events for their index. This
keeps the write frequency accurate across global event-number gaps, cold starts, and
concurrent commands. Snapshots remain disposable; the event log is the source of truth.

`closeSnapshotWriter backend` stops admission, discards pending work, and cancels/joins
the active worker. It is idempotent, does not drain, and does not destroy caller-owned
pools. A cancelled store may already have committed a snapshot. Event operations remain
usable after closing, but no further snapshots are written by that backend.

For tests or an explicit best-effort wait, use `flushSnapshots timeout backend`. It returns
`SnapshotFlushCompleted`, `SnapshotFlushTimedOut`, or `SnapshotFlushClosed`. Completion
means queued attempts have finished, not that they succeeded; it neither forces a
below-cadence snapshot nor closes admission. Concurrent traffic can extend the wait.
A flush timeout leaves the worker running. Snapshot-disabled backends flush immediately
and closing them is a no-op.

The snapshot timeout (default five seconds) applies separately to each synchronous
load/decode, exact invalid-snapshot deletion, or background encode/store attempt; it does
not bound the entire request. Store initialization happens during construction outside
this timeout, and initialization failures propagate. Timeouts and shutdown cancellation
require custom codecs/stores to cooperate with asynchronous exceptions. Failures and
rejected snapshots are logged using the backend that triggered the work; custom loggers
must return promptly. Sharing a pool with event operations can still cause connection
contention; a separate snapshot pool is supported through `postgresSnapshotStore`.
