# Changelog for domaindriven

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
