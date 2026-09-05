# domaindriven-core

This library contains the core model and persistance handlers for domaindriven.

## Optional PostgreSQL snapshots

PostgreSQL models can opt into snapshots without changing the event log, which remains
the source of truth:

```haskell
frequency <- maybe (fail "invalid snapshot frequency") pure (mkEveryNEvents 100)
let snapshots =
        SnapshotConfig
            { snapshotCodec = serialiseCborSnapshotCodec
            , snapshotFrequency = frequency
            , snapshotTimeout = defaultSnapshotTimeout
            , snapshotStore = postgresSnapshotStore pool
            }
backend <-
    postgresWriteModelWithSnapshots
        pool
        eventTable
        snapshots
        applyEvent
        initialModel
```

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

Snapshots are keyed by the physical event-table version and aggregate index. A migration
therefore causes a snapshot miss and a complete reconstruction while `getEventList` and
`getEventStream` continue to expose the full event history. Also bump/migrate the physical
event table whenever the seed, event application semantics, model serialization, or a
compatible-looking model meaning changes; a snapshot that still decodes cannot detect
such semantic changes.

The existing `postgresWriteModel` and `postgresWriteModelNoMigration` constructors remain
snapshot-free and do not create the shared `public."domaindriven-snapshots"` table.

A due snapshot is encoded and stored synchronously after the event commit and can delay
the response by at most `snapshotTimeout`; failure is logged and later traffic retries.
Changing an event table between JSON and CBOR must be coordinated across all running
instances because the table/index has one latest snapshot row. Mixed deployments can
otherwise invalidate and rebuild each other's snapshots repeatedly.
