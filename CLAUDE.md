# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

- **Build all packages**: `stack build`
- **Build specific package**: `stack build domaindriven-effectful`
- **Run tests**: `stack test`
- **Run specific test**: `stack test domaindriven-effectful`
- **Clean build**: `stack clean`

## Architecture Overview

DomainDriven is a synchronous event sourcing and CQRS library split into multiple packages:

### Core Components

- **domaindriven-core**: Core persistence model with PostgreSQL and in-memory backends
  - `ReadModel`/`WriteModel` type classes define the persistence interface
  - `ForgetfulInMemory`: In-memory backend for testing/development
  - `Postgres`: Production persistence with transactional guarantees
  - Synchronous event sourcing with locks to avoid eventual consistency issues

- **domaindriven**: Main library using GADTs and Template Haskell for API generation
  - Custom Servant combinators (`Cmd`, `Query`, `CbCmd`, `CbQuery`)
  - `DomainDrivenApi` wrapper for automatic route generation
  - Server interpreters that connect to persistence backends

- **domaindriven-effectful**: Experimental Effectful-based implementation
  - Uses standard Servant combinators instead of custom ones
  - Two main effects: `Aggregate` (commands) and `Projection` (queries)
  - Dynamic dispatch interpreters for different backends
  - Located in `domaindriven-effectful/`

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

- All packages use extensive language extensions (see package.yaml files)
- Strict warning settings (`-Wall -Werror`) - fix all warnings before committing
- The Effectful prototype aims to simplify the API while maintaining type safety
- Tests use `hspec` framework with in-memory backends