# Changelog for domaindriven

## 0.7.0

- **Breaking:** `EventTable` is now `MigrateWith MigrationTag EventMigration EventTable`
  / `TableName EventTableBaseName EventTableVersion`. Table names are unchanged,
  so existing databases are picked up as is (pre-0.7 tables are adopted
  automatically on the first 0.7 startup; no manual step). To upgrade, keep the
  chain length and rewrite the constructors:

  ```haskell
  -- 0.6
  eventTable = MigrateUsing migrationV3 $ MigrateUsing migrationV2 $ InitialVersion "events"
  -- 0.7: current table is still events_v3
  eventTable =
      MigrateWith "v3-split-name" migrationV3
          $ MigrateWith "v2-add-email" migrationV2
          $ TableName "events" 1
  ```

  Tags must be non-empty and unique per base name, and should not be renamed
  once deployed (that requires an `update domaindriven_migrations` by hand).
- Applied migrations are recorded by tag in a new `domaindriven_migrations`
  table and verified at startup; a chain that disagrees with the database
  (wrong tag, off-by-one `TableName` version, database ahead of or behind the
  code) fails fast with a `MigrationError` instead of migrating.
- Old migrations can be deleted from code once every database is past them:
  remove the `MigrateWith` and bump the `TableName` version by one per removed
  step. `discardedMigration` placeholders are no longer needed.

  ```haskell
  MigrateWith "v3-split-name" migrationV3 $ MigrateWith "v2-add-email" migrationV2 $ TableName "events" 1
  -- once every database is at v2 or later:
  MigrateWith "v3-split-name" migrationV3 $ TableName "events" 2
  ```

  A database still at a deleted version refuses to start (`DatabaseBehindCodeBase`).
- Fresh databases get only the current table; migration functions do not run
  on them, so put any setup a migration used to do (extra indexes, say)
  elsewhere. Upgrade the whole fleet before provisioning fresh databases with
  0.7 or bumping `TableName` versions.
- Migrations now lock the previous table against writers on every index
  (previously `Indexed` writers could strand events in the retired table).
- New: `dropEventTables` (resets a chain and its metadata; for tests/dev
  databases), `validateEventTable`; `LogEntry` gains `WaitingForMigrationLock`,
  `EventTableBootstrapped`, `EventTableAdopted` (add cases to exhaustive custom
  loggers); the unused `EventVersion` alias is replaced by `EventTableVersion`.

## 0.6.1

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
