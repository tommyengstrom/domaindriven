# Changelog for domaindriven

## 0.7.0

- **Breaking:** parameterize PostgreSQL index values and remove
  `IsPgIndex.toQuery`; reject index values containing NUL bytes.
- **Breaking:** derive advisory lock keys in PostgreSQL: commands hold the
  table key shared and their index key exclusive, and migrations take the table
  key exclusively instead of `LOCK TABLE` (no table privilege needed).
  In-flight commands complete and are copied when a migration starts; a command
  nested on the same table can hang undetected if a migration queues in between
  (bound it with `lock_timeout`). Writers sharing a database must all run the
  same domaindriven-core version — 0.6 and 0.7 lock keys do not conflict with
  each other.
- **Breaking:** simplify the internal PostgreSQL event-query API.
- **Breaking:** serialize `ForgetfulInMemory` commands per index and update its
  model and history atomically.
- Avoid unnecessary transactions for current cached models and prevent
  duplicate `(index, event_number)` indexes.
- Make indexed-table migrations safe for concurrent writers and starts, and
  parse migrated events in parallel.
- Enforce PostgreSQL's 63-character event-table name limit.
- Normalize stored timestamps to PostgreSQL microsecond precision.
- Document the locking and sequence requirements for `writeEvents`.
- Include caller locations when logging `getEventList` connection waits.

## 0.6.1

- Switched the build from Stackage LTS 24.31 to Nightly 2026-08-10 with GHC
  9.12.4.
- PostgreSQL advisory lock keys now include the resolved event table name as
  well as the aggregate index, so equal indices in unrelated tables no longer
  block each other. Because this changes the lock-key protocol, all writers
  sharing a database should be upgraded together.
- PostgreSQL indexed-model freshness checks now use a parameterized `EXISTS`
  query over `(index, event_number)`, and writes derive their watermark from
  `INSERT ... RETURNING` instead of scanning the whole event table. Commit
  failures are propagated and model caches are updated strictly, monotonically,
  and only after a successful write commit.
- Existing PostgreSQL deployments should verify that every active event table
  has an index on `(index, event_number)`. For large tables, consumers should
  create a missing index with `CREATE INDEX CONCURRENTLY`; startup migrations
  intentionally continue to avoid blocking index replacement.

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
