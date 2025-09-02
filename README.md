# DomainDriven

DomainDriven is a batteries included synchronous event sourcing and CQRS library. It is split into two parts:

- [domaindriven-core](domaindriven-core) Contains the core persistance model as well as postgres and in-memory backend.
- [domaindriven](domaindriven) Introduces a convenient way of specifying actions using GADTs and TemplateHaskell.
- [domaindriven](domaindriven-examples) Examples of how to use domaindriven.


## Design idea

The core idea it to do synchronous event sourcing with locks and thereby provide the upsides of event sourcing without the extra complexity introduced by asynchrnous workflows.


