# DomainDriven Effectful Examples

Example applications demonstrating the Effectful-based domaindriven library.

## Examples

### Simple Counter
Basic counter application using Effectful effects with standard Servant API.

Run with:
```bash
cabal run simple-example
```

### Postgres Example
Counter application with PostgreSQL persistence and event migration.

Run with:
```bash
cabal run postgres-example
```

## Features Demonstrated

- Using standard Servant combinators (`Get`, `Post`) instead of custom ones
- Effectful effects for domain logic
- In-memory and PostgreSQL persistence backends
- Type-safe effect composition
- Event versioning and migration
