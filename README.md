# DomainDriven

DomainDriven is a batteries included synchronous event sourcing and CQRS library. It is split into the following packages:

- [domaindriven-core](domaindriven-core) - Core persistence model with PostgreSQL and in-memory backends.
- [domaindriven](domaindriven) - Effectful-based API layer with `Aggregate` and `Projection` effects, plus Servant integration.
- [domaindriven-examples](domaindriven-examples) - Example applications demonstrating usage.
- [servant-reqbody-field](servant-reqbody-field) - Servant combinator for decoding individual fields from one shared JSON object request body.

## Design idea

The core idea is to do synchronous event sourcing with locks and thereby provide the upsides of event sourcing without the extra complexity introduced by asynchronous workflows.

## `servant-reqbody-field` dependency graphs

The root project follows Stackage Nightly 2026-08-10, which selects
`insert-ordered-containers-0.3.0`. It therefore uses the `servant-openapi3` fork
pinned in `cabal.project` for compatibility with that version.

That source override is only part of the monorepo development graph. A
standalone Hackage build of the `servant-reqbody-field` source distribution
uses stock `servant-openapi3-2.0.2.0`, whose bound selects
`insert-ordered-containers-0.2.7`.
