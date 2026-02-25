# DomainDriven

DomainDriven is a batteries included synchronous event sourcing and CQRS library. It is split into the following packages:

- [domaindriven-core](domaindriven-core) - Core persistence model with PostgreSQL and in-memory backends.
- [domaindriven-effectful](domaindriven-effectful) - Effectful-based API layer with `Aggregate` and `Projection` effects, plus Servant integration.
- [domaindriven-effectful-examples](domaindriven-effectful-examples) - Example applications demonstrating usage.

## Design idea

The core idea is to do synchronous event sourcing with locks and thereby provide the upsides of event sourcing without the extra complexity introduced by asynchronous workflows.
