# DomainDriven Examples

Example applications demonstrating the `domaindriven` library.

The examples now come in two persistence families:

- Legacy event-sourced state:
  `simple-example`, `fieldname-as-path-example`, `postgres-example`, `crm-example`
- Beam-backed relational projections:
  `simple-beam-example`, `fieldname-as-path-beam-example`, `postgres-beam-example`, `crm-beam-example`

## In-Memory Examples

### Simple Counter (`simple/`)
Getting started example with in-memory persistence. No database required.

Features: model/event/applyEvent pattern, `Aggregate` and `Projection` effects, `GET /events`.

```bash
cabal run simple-example
```

### FieldNameAsPath (`fieldname-as-path/`)
Same counter domain as `simple/`, but record field names become URL paths automatically.

```bash
cabal run fieldname-as-path-example
```

### CRM (`crm/`)
Three-level customer/order/item example using the in-memory backend.

```bash
cabal run crm-example
```

## PostgreSQL Event Backend

### PostgreSQL + Event Migration (`postgres/`)
Counter with PostgreSQL persistence and event schema evolution.

Features: `simplePool`, `postgresWriteModel`, `MigrateUsing`, and `ShapeCoercible` event migration from V1 to V2.

```bash
cabal run postgres-example
```

## Beam Projection Backends

All Beam examples require a running PostgreSQL instance:

```bash
createdb -U postgres domaindriven
```

### Simple Counter (`simple-beam/`)
Counter backed by a Beam projection table plus the normal event table.

```bash
cabal run simple-beam-example
```

### FieldNameAsPath (`fieldname-as-path-beam/`)
Same API as the FieldNameAsPath counter, but backed by Beam projection tables.

```bash
cabal run fieldname-as-path-beam-example
```

### PostgreSQL + Event Migration (`postgres-beam/`)
Same migrated counter domain as `postgres/`, but with Beam-managed relational state rebuilt from the newest event table.

```bash
cabal run postgres-beam-example
```

### CRM (`crm-beam/`)
Beam-backed CRM projection using relational tables for customers, orders, and items.

```bash
cabal run crm-beam-example
```
