# Changelog for domaindriven

## 0.6.1

- Added opt-in PostgreSQL model snapshots with strict Aeson JSON and typed
  `serialise`/CBOR codecs. Existing PostgreSQL constructors remain
  snapshot-free; use `postgresWriteModelWithSnapshots` or its no-migration
  counterpart to enable them.
- **Breaking for direct backend construction:** `PostgresEvent` and its cache
  metadata gained snapshot runtime fields. Constructor functions are source
  compatible, but code constructing or positionally matching the exported
  record must be updated.
- PostgreSQL command checkpoints now come directly from `INSERT ... RETURNING`,
  freshness checks are aggregate-index-specific, committed cache publications
  are merged monotonically, and commit failures are propagated.
- Snapshot identities now include explicit projection names/revisions and stable
  codec IDs, allowing multiple projections, encodings, and rolling deployments
  without event-table migrations. `SnapshotConfig` requires projection identity
  and queue capacity; `customSnapshotCodec` requires a codec ID. All snapshot
  identities share the unversioned `public."domaindriven-snapshots"` table.
- Due snapshot encoding/storage now runs in a bounded, coalescing FIFO background
  writer (default 64 pending indexes). Added bounded `flushSnapshots` and
  idempotent, discard-on-close `closeSnapshotWriter`; close writers before pools.
- Snapshot review fixes preserve binary CBOR payloads and Unicode indexes,
  release connections on failed transaction acquisition, and keep persisted
  per-index event counts and snapshot progress monotonic during concurrent commands.
- Snapshot timeouts reject unrepresentable durations, rejected snapshots log
  their cause, and batch inserts return only the final checkpoint.
- Switched the build from Stackage LTS 24.31 to Nightly 2026-08-10 with GHC
  9.12.4.
- PostgreSQL advisory lock keys now include the resolved event table name as
  well as the aggregate index, so equal indices in unrelated tables no longer
  block each other. Because this changes the lock-key protocol, all writers
  sharing a database should be upgraded together.

## 0.6.0

- **Breaking:** event types now require an `NFData` instance. This is enforced
  uniformly via a superclass on `ReadModel` (and therefore `WriteModel`), so it
  applies to every backend. For most event types `deriving (Generic, NFData)`
  is enough.
- Postgres event parsing is now performed in parallel across worker threads.
  The number of parser workers is configurable via the new `parseConcurrency`
  field on `PostgresEvent` (defaults to `getNumCapabilities`); parsed events are
  fully forced (`NFData`) off the consuming thread. Requires a threaded runtime
  (`-threaded -with-rtsopts=-N`) to benefit; harmless otherwise. Each fetched
  batch is split into `chunkSize \`div\` parseConcurrency`-row tasks across the
  workers, so `chunkSize` now governs both the Postgres round-trip size and the
  parse-task granularity. The default `chunkSize` was raised from 50 to 2048 so
  that parallel parsing engages on the streaming/refresh read paths out of the
  box.
- Postgres now selects the event column as `event::text` and decodes from a
  strict `ByteString`. For `jsonb` columns this means the decoded bytes are
  Postgres's normalized JSON (keys reordered, whitespace stripped, numbers
  re-rendered) — semantically identical for any `FromJSON` instance, but worth
  noting for byte-sensitive consumers.

## 0.5.0

First release published on hackage.
