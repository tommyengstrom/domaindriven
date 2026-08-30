# Review findings and work plan

Review of correctness, performance, test coverage and benchmarks (2026-08-30).
Commit `48b8985` ("Fix indexed PostgreSQL hot paths") already fixes the
indexed-model freshness check, the `max(event_number)` scan and the
commit-failure/cache-poisoning bug; those items are checked off below.

## Phase 0 — dedicated Postgres and connection config

- [x] `flake.nix`: add `pkgs.postgresql_17` to the dev shell.
- [x] `process-compose.yaml`: `0-postgres` process (per-worktree data dir
      `.pc-postgres/data`, unix socket only → no port clashes between
      worktrees, `fsync=off`), `1-postgres-db` one-shot `createdb`; tests and
      benchmarks depend on it and get `PGHOST`/`PGUSER`/`PGDATABASE`.
- [x] `.gitignore`: `.pc-postgres/`.
- [x] Tests connect with `connectPostgreSQL` honouring libpq `PG*` env vars,
      falling back to `localhost:5432/postgres/postgres/domaindriven` (CI).
- [x] CI: pin `postgres:17`, set `PG*` env, build with `--enable-benchmarks`.

## Phase A — domaindriven-core fixes

- [x] **A1 (C1, security)** `toQuery` interpolated `Indexed` text unescaped into
      the read queries; `query_` uses the simple protocol so
      `Indexed "x'; drop table …; --"` executed, and an index containing `'`
      could be written (parameterized) but never read back. Every value now
      goes through libpq escaping (`formatQuery` for the cursor query), NUL
      bytes are rejected, and `toQuery` is gone.
- [x] **A2 (P1)** `queryHasEventsAfter` was table-global and `count(*) > 0`:
      every indexed `getModel` after any write to another index took the
      exclusive lock and re-streamed, forever. → `48b8985`.
- [x] **A3 (P2)** `writeEvents` computed a full-table `max(event_number)`
      (seq scan per command). → `INSERT … RETURNING` in `48b8985`.
- [x] **A4 (C2)** Commit failures were swallowed and the cache was published
      before COMMIT: `runCmd` reported success with unpersisted events and the
      cache stayed poisoned until restart. → `48b8985`.
- [x] **A5 (P4)** Cache-hit `getModel` cost pool checkout + BEGIN + `exists`
      + COMMIT; now one autocommit `exists`, a transaction only to refresh.
- [x] **A6 (P3)** `createEventTable'` ran an unnamed `create index on …`, so
      every `postgresWriteModelNoMigration` call (each process start) added a
      duplicate index. Now checks `pg_indexes` for an `(index, event_number)`
      index first (name-independent, so Postgres's truncation of long
      auto-generated names does not matter); table names capped at 63 chars.
- [x] **A7 (C5)** Advisory-lock key was `hashable`'s `hash (table, index)`:
      mixed `hashable` versions across writers silently lost mutual exclusion.
      Now `pg_advisory_xact_lock(hashtextextended(index, hashtextextended(table, 0)))`
      (64-bit, computed by the DB). Lock-protocol change → stop all writers
      before upgrading (documented in haddock + ChangeLog).
- [x] **A8 (C4)** `toStored` used a ns clock; Postgres rounds to µs, so the
      `Stored` given to `applyEvent`/`postUpdateHook` in-process differed from
      the replayed one. Now truncated to µs in `toStored`.
- [x] **A9 (C3, C6c, P6)** Migrations: the previous table is locked with `lock
      table … in exclusive mode` (the `NoIndex` advisory key did not block
      `Indexed` writers → events committed during the copy were lost); an
      advisory lock on the new table name precedes the existence check
      (concurrent first starts); migrated events are parsed across all
      capabilities with a 2048-row chunk (was 100).
- [ ] **A9b (P6, optional)** Migrate with one cursor ordered by
      `(index, event_number)` instead of one cursor per index (needs the index
      column in the streamed row type).
- [x] **A10 (C6a, P5)** `ForgetfulInMemory`: one global `QSem` (nested `runCmd`
      on another index deadlocked in-memory but worked on Postgres) → per-index
      STM busy set; `[Stored e] <> new` was O(n²) → `Seq`; model and history
      updated together with one `atomicModifyIORef'`.
- [x] Document on `WriteModel`/`writeEvents`: all writers must hold the
      `(table, index)` advisory lock from read to COMMIT (a raw `writeEvents`
      in an open transaction concurrent with `runCmd` yields an event below the
      watermark that is never applied); the identity sequence must keep
      `CACHE 1`; an async exception during COMMIT surfaces as an error although
      the server may have committed (next read self-heals).
- [x] ChangeLog: `toQuery` removal, lock key + upgrade procedure, watermark
      precondition, timestamp truncation, `ForgetfulInMemory` record changes,
      removed `Internal` helpers; bump to 0.7.0 (breaking).

## Phase B — tests

- [x] Quoting round trip through every Postgres read path (`indexedSpec`) and
      nested cross-index transaction in `ForgetfulInMemory` (`InMemorySpec`).
- [ ] Backend-parametric spec (`WriteModelSpec`) over `AnyWriteModel`, run
      against `ForgetfulInMemory` and `PostgresEvent`: result/model/list/stream
      agreement; per-index isolation; failing command persists nothing and
      releases lock/pool.
- [ ] Two `PostgresEvent` instances over one table converge (multi-process
      model), sequential and concurrent (`sort results == [2..21]`).
- [ ] Connection killed mid-command (`pg_terminate_backend` from the callback):
      `runCmd` throws, hook not fired, pool recovers, cache untouched.
- [ ] Nested `getModel A` inside a command on A does not deadlock (`timeout`).
- [ ] Monotone-watermark stress: concurrent writers/readers + sampler; sampled
      watermarks non-decreasing; final cache == DB.
- [x] `postgresWriteModelNoMigration` twice → `pg_indexes` count stays 2 (P3).
- [x] Indexed-table migration under concurrent `Indexed` writers: old/new
      `count(*)` equal or writer fails with "retired" (C3).
- [x] Concurrent first-start migration: no duplicates, no crash.
- [ ] Abandoned `getEventStream` (pool size 1, `Stream.take 1`, then
      `getModel` under `timeout`) — characterization of GC-driven cleanup.
- [ ] `parseEventRows`: `[]`, `chunkSize < workers`.
- [x] Timestamp round trip: hook/`runCmd` `Stored` == DB `Stored` (C4).
- [ ] Precondition doc test (`pending`): raw `writeEvents` in an open
      transaction concurrent with `runCmd` on the same index.
- [ ] Consider shrinking the 500k-row `EXPLAIN` test to 100k or tag it slow.
- [ ] `domaindriven`: `AnyWriteModel` polymorphism test; `FieldNameAsPath`
      routing tests (two fields with identical types; newtype record;
      `HasClient` path == server route; custom `apiTagFromLabel`; `ThrowAll`).
- [ ] `shape-coerce`: new test suite — identity coercions incl. `Map Text Int`
      (**confirmed**: `Map k v → Map k v` currently fails with overlapping
      instances), cross-module round trip (QuickCheck), `Stored a` metadata
      preserved, nested containers incl. `Map` size preservation, pinned
      positional-swap limitation; should-not-compile fixtures via
      `ghc -fno-code` with a regex on the custom `TypeError` text.
- [ ] `servant-reqbody-field`: client `Just` value with an existing non-object
      body must fail loudly; OpenAPI `ReqBodyField` + `ReqBody` (`Ref` body);
      multi-field failure ordering; client↔server round trip.

## Phase C — benchmarks

- [ ] Delete `benchmark/bench/` (pre-Effectful `mkServer` API, dead since 2023,
      referenced by nothing).
- [ ] New `domaindriven-core` benchmark `postgres-persistence` (criterion, uses
      the Phase 0 connection helper, `--seed N K`): `runCmd/1-event` at
      N ∈ {1e3, 1e5}; `getModel/cache-hit/after-other-index-write`;
      `getModel/cache-hit/quiet`; `refreshModel/full-replay` N=1e5 ×
      parseConcurrency {1, caps} × chunkSize {256, 2048, 8192};
      `getEventStream` vs `getEventList`; `migrate1to1` 1e5 events / 1e3
      indices; print `EXPLAIN (ANALYZE)` of the hot queries at the seeded
      cardinality.
- [ ] In-memory benchmark: `ForgetfulInMemory` `runCmd` throughput.
- [ ] `servant-reqbody-field` benchmark baseline: hoist `fieldKeys` per `n`
      into a CAF (the `ReqBody` arm currently recomputes keys per parse).
- [ ] Harness: `-O1 --builddir dist-bench` benchmark builds, criterion
      `--time-limit 2` (benchmarks already run after tests since Phase 0).

## Phase D — code and docs cleanup

- [ ] `FieldNameAsPath`: doc comment states the wrong path order and uses a
      removed `Cmd` type; newtype API records are rejected (`'Newtype` not
      matched in `GenericRecordFieldInfos'`); client path segments are not
      URL-encoded; `hoistFieldNameAsPathServer` hard-codes context `'[]`.
- [ ] `shape-coerce`: add `{-# OVERLAPPING #-} Ord k => ShapeCoercible (Map k v)
      (Map k v)`; assert `Map` size preservation in the `Map` instance.
- [ ] `servant-reqbody-field`: fail loudly in `mergeClientField` for
      non-object bodies; merge `Ref` request bodies in OpenAPI (or document);
      optional decode-once client merge (currently O(fields²)).
- [ ] Docs: remove non-existent `emitEvents` (`app-wiring.md`); complete the
      `DomainDriven` re-export list in `SKILL.md`; document `postUpdateHook`,
      `Indexed` validity, lock-protocol caveat, commit-failure semantics,
      `zoom*`/`hoist*` helpers; fix `CLAUDE.md` (fundeps; "tests use in-memory
      backends"); `domaindriven-examples/README.md` says three examples.
- [ ] `crm/Server.hs:148-154`: second `getModel` after the transaction
      (TOCTOU) → use `returnFn`.
- [ ] Add an `Indexed`-from-`Capture` example.

## Follow-up (out of scope)

- [ ] Lock-free refresh (readers never wait for in-flight commands) with a
      per-index in-process single-flight to avoid thundering-herd refreshes.
- [ ] `refreshModel`: re-check `EXISTS` after taking the lock (a burst of
      readers each declares an empty cursor today) and use a plain query
      instead of a cursor for small deltas (4 round trips → 1).
- [ ] `withIOTrans` could be built on `withPooledConnection` (rollback inside
      the bracket, ~50 fewer lines).
- [ ] `runMigrations`: `to_regclass` instead of the `information_schema` join
      for the existence check.
- [ ] Optional transitional dual lock (legacy `hashable` key + new key) if a
      rolling 0.6 → 0.7 upgrade is ever required.
