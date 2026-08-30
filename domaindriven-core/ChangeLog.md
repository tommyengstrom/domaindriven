# Changelog for domaindriven

## 0.7.0

- **Breaking:** `IsPgIndex` no longer has a `toQuery` method. Index values were
  interpolated unescaped into the read queries; every value now reaches SQL
  escaped by libpq (`formatQuery`), so indices containing quotes round-trip and
  cannot inject SQL. Index values containing a NUL byte are rejected with
  `ValueError` (libpq would silently truncate them).
- **Breaking:** the advisory lock key is now computed by PostgreSQL
  (`pg_advisory_xact_lock(hashtextextended(index, hashtextextended(table, 0)))`)
  instead of by `hashable`, so it no longer depends on the dependency versions
  of each writer. Writers on 0.6 and 0.7 do not exclude each other on the same
  index: stop every writer sharing the database before starting the first 0.7
  writer. A rolling upgrade would let two commands on one aggregate run at once.
- **Breaking:** `mkEventQuery`, `mkEventsAfterQuery`, `EventQuery`,
  `mkEventStream`, `queryEventsAfter` and `queryEventsAfterWithParseConcurrency`
  are gone from `DomainDriven.Persistance.Postgres.Internal`;
  `mkEventStreamWithParseConcurrency` now takes the table, index and the event
  number to start after, and renders the cursor query itself.
- **Breaking:** `ForgetfulInMemory` serializes commands per index, like the
  Postgres backend, instead of through one global lock, and keeps model and
  history together in `stateRef`; the `lock` and `events` fields are gone and
  `busyIndices` added. `createForgetful` is unaffected.
- `getModel` on the Postgres backend no longer opens a transaction when the
  cached model is current: it runs one `EXISTS` statement on a pooled
  connection and only starts a transaction to refresh. Cache hits therefore no
  longer produce `DbTransactionDuration` log entries.
- `createEventTable'` only creates the `(index, event_number)` index when the
  table has none, so calling `postgresWriteModelNoMigration` on every start no
  longer accumulates duplicate indexes. Deployments that restarted often can
  drop their extra ones:
  `select 'drop index concurrently ' || quote_ident(schemaname) || '.' || quote_ident(indexname) || ';' from pg_indexes where indexname ~ '_index_event_number_idx[0-9]+$';`
- Event table names are limited to 63 characters, the PostgreSQL identifier
  limit; longer names were silently truncated by the server.
- Migrations lock the previous table in `EXCLUSIVE` mode while copying, which
  blocks writers of indexed tables too (the previous advisory lock only covered
  `NoIndex`), and take an advisory lock on the target table name before checking
  whether it exists, so concurrent first starts no longer race. Migrated events
  are parsed in parallel across all capabilities (previously single-threaded),
  and the migration chunk size is now the read chunk size (2048, was 100).
- `toStored` truncates timestamps to microseconds, matching what PostgreSQL
  stores, so the events handed to `applyEvent` and the update hook are equal to
  the replayed ones. Compare stored timestamps against reference times that
  went through `truncateToMicroseconds` as well.
- `writeEvents` documents its precondition: callers must hold the
  `(table, index)` advisory lock from before reading the model until commit,
  and the identity sequence must keep `CACHE 1`; otherwise a cached model can
  miss an event forever.
- `getEventList` carries a `HasCallStack` constraint so its connection wait is
  logged with the caller's location, like the other read paths.

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
