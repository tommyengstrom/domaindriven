# DomainDriven Effectful Examples

Example applications demonstrating the Effectful-based domaindriven library.
All three examples use a simple counter domain to keep the focus on the library features.

## Examples

### 1. Simple Counter (`simple/`)
Getting started example with in-memory persistence. No database required.

Features: model/event/applyEvent pattern, Aggregate & Projection effects, `GET /events` endpoint.

```bash
cabal run simple-example          # starts on port 7878
curl localhost:7878               # get counter value
curl -X POST localhost:7878/increase
curl localhost:7878/events        # list stored events
```

### 2. PostgreSQL + Event Migration (`postgres/`)
Counter with PostgreSQL persistence and event schema evolution.

Features: `simplePool` connection pooling, `postgresWriteModel`, event migration from V1 (unit events) to V2 (events with Int payload) via `ShapeCoercible` and `MigrateUsing`.

Requires a running PostgreSQL instance:
```bash
createdb -U postgres domaindriven
```

```bash
cabal run postgres-example        # starts on port 7879 (requires PostgreSQL)
curl -X POST -H 'Content-Type: application/json' -d '5' localhost:7879/increase
curl localhost:7879
```

### 3. FieldNameAsPath (`fieldname-as-path/`)
Same counter as `simple/`, but uses `FieldNameAsPathApi` so record field names become URL paths automatically — no explicit path strings needed.

```bash
cabal run fieldname-as-path-example  # starts on port 7880
curl localhost:7880/get
curl -X POST localhost:7880/increase
```
