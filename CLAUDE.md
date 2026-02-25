# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

- **Build all packages**: `cabal build all`
- **Build specific package**: `cabal build domaindriven`
- **Run tests**: `cabal test all`
- **Run specific test**: `cabal test domaindriven`
- **Clean build**: `cabal clean`

## Architecture Overview

DomainDriven is a synchronous event sourcing and CQRS library split into multiple packages:

### Core Components

- **domaindriven-core**: Core persistence model with PostgreSQL and in-memory backends
  - `ReadModel`/`WriteModel` type classes define the persistence interface
  - `ForgetfulInMemory`: In-memory backend for testing/development
  - `Postgres`: Production persistence with transactional guarantees
  - Synchronous event sourcing with locks to avoid eventual consistency issues

- **domaindriven**: Effectful-based API layer
  - Uses standard Servant combinators instead of custom ones
  - Two main effects: `Aggregate` (commands) and `Projection` (queries)
  - Dynamic dispatch interpreters for different backends
  - Located in `domaindriven/`

### Key Design Patterns

1. **Event Sourcing Model**:
   - Model = current state (derived from events)
   - Events = immutable history of changes
   - `applyEvent :: Model -> Stored Event -> Model`

2. **Command/Query Separation**:
   - Commands emit events and may update state
   - Queries are read-only operations
   - Both can be composed hierarchically

3. **Index Types**:
   - `NoIndex` for single aggregates
   - Custom index types for multiple aggregates
   - Functional dependencies can reduce type parameters

## Development Notes

- All packages use extensive language extensions (see .cabal files)
- Strict warning settings (`-Wall -Werror`) - fix all warnings before committing
- The Effectful layer aims to simplify the API while maintaining type safety
- Tests use `hspec` framework with in-memory backends