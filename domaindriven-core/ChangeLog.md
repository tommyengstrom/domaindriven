# Changelog for domaindriven

## 0.7.0

- **Breaking:** parameterize PostgreSQL index values and remove
  `IsPgIndex.toQuery`; reject index values containing NUL bytes.
- **Breaking:** derive advisory lock keys in PostgreSQL: commands hold the
  table key shared and their index key exclusive, and migrations take the table
  key exclusively instead of `LOCK TABLE` (no table privilege needed).
  In-flight commands complete and are copied when a migration starts; a command
  nested on the same table can time out if a migration queues in between.
  Commands use a transaction-local 60-second `lock_timeout` when the
  connection has no finite timeout configured. Writers sharing a database must
  all run the same domaindriven-core version — 0.6 and 0.7 lock keys do not
  conflict with each other.
- **Breaking:** simplify the internal PostgreSQL event-query API.
- **Breaking:** serialize `ForgetfulInMemory` commands per index and update its
  model and history atomically.
- **Breaking:** `EventTable` is now `MigrateTo EventTableVersion EventMigration EventTable`
  / `TableName EventTableBaseName EventTableVersion`: every step states the version it
  produces. Table names are unchanged, so existing databases are picked up as they are.
  To upgrade, number each step after the table it produces, and replace any
  `discardedMigration` placeholders by raising the `TableName` version to the oldest
  version that is still migrated from:

  ```haskell
  -- 0.6
  eventTable = MigrateUsing migrationV3 $ MigrateUsing migrationV2 $ InitialVersion "events"
  -- 0.7: current table is still events_v3
  eventTable = MigrateTo 3 migrationV3 $ MigrateTo 2 migrationV2 $ TableName "events" 1

  -- 0.6, with placeholders for v2 and v3
  eventTable =
      MigrateUsing migrationV4 $ MigrateUsing discardedMigration $ MigrateUsing discardedMigration
          $ InitialVersion "events"
  -- 0.7: current table is still events_v4
  eventTable = MigrateTo 4 migrationV4 $ TableName "events" 3
  ```

  Before the first deploy, check that `getEventTableName eventTable` is the name of
  your live table: a chain numbered one too high runs its newest migration again.
- **Breaking:** startup refuses a database it cannot bring to the chain's current
  version: one whose newest table is above the chain (`DatabaseAheadOfCode`, so a build
  older than the database no longer starts) or below the `TableName` version
  (`DatabaseBelowBaseVersion`). The `DatabaseAheadOfCode` message says how to roll the
  database back instead.
- **Breaking:** a database without tables for the base name gets only the current table.
  Migration functions do not run on it, so put any setup a migration used to do (extra
  indexes, say) elsewhere.
- **Breaking:** PostgreSQL 13 or later is required (`pg_current_xact_id_if_assigned`).
- Enforce PostgreSQL's 63-character event-table name limit in both constructors,
  including `postgresWriteModelNoMigration`, before acquiring a connection. Reads and
  commands also validate names supplied through backend record updates.
- **Breaking:** invalid raw table names now throw `MigrationError`
  (`InvalidEventTableName` or `EventTableNameTooLong`) instead of `ErrorCall`.
  `getEventTableName` only renders a name; use `validateEventTable` to check a chain.
  The internal `validateEventTableName` now returns `MonadThrow m => m ()`.
- Avoid unnecessary transactions for current cached models and prevent
  duplicate `(index, event_number)` indexes.
- Make indexed-table migrations safe for concurrent writers and starts, and
  parse migrated events in parallel.
- Normalize stored timestamps to PostgreSQL microsecond precision.
- Document the locking and sequence requirements for `writeEvents`.
- Include caller locations when logging `getEventList` connection waits.
- Steps must be numbered consecutively from one above the `TableName` version.
  `postgresWriteModel` rejects other chains with a `MigrationError` before connecting,
  and `validateEventTable` runs the same check (including the name limit) without a
  database.
- Old migrations can be deleted from code once every database is past them: remove the
  `MigrateTo` and raise the `TableName` version to one below the oldest remaining step.
  `discardedMigration` placeholders are no longer needed.

  ```haskell
  MigrateTo 3 migrationV3 $ MigrateTo 2 migrationV2 $ TableName "events" 1
  -- once every database is at v2 or later:
  MigrateTo 3 migrationV3 $ TableName "events" 2
  ```
- Event tables are found through the search path, like every other statement, instead
  of only in `public`. Version discovery uses the first schema containing tables for
  the base name, and migrations create new tables and retirement functions in that
  same schema. Tables in fallback schemas do not affect version or retirement checks.
- A migration function that commits or rolls back the transaction it is given (with
  `withTransaction`, say) fails startup with `MigrationEndedTransaction`; the previous
  table stays live.
- Startup checks that the previous existing table has an enabled retirement trigger
  before accepting the newest version. An incomplete transition fails with
  `IncompleteMigration`, including on retries or concurrent starts after a migration
  accidentally commits. This checks only the previous existing version.
- New: `postgresWriteModelWith` applies a modifier (a custom `logger`, for instance)
  before migrations run; `validateEventTable`; `getEventTableName`, `eventTableNameFor`,
  `LogEntry` and `maxEventTableNameLength` are exported from
  `DomainDriven.Persistance.Postgres`; `LogEntry` gains `WaitingForMigrationLock` (add a
  case to exhaustive custom loggers); the unused `EventVersion` alias is replaced by
  `EventTableVersion`.

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
