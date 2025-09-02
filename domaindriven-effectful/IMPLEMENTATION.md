# DomainDriven Effectful Implementation

This is a prototype implementation of domaindriven using the Effectful library instead of custom Servant combinators.

## Key Changes from Original

### 1. Standard Servant Combinators
- Uses `Get`, `Post` instead of custom `Query`, `Cmd` combinators
- Cleaner separation between HTTP layer and domain logic

### 2. Effect-based Architecture
Two main effects:
- `Projection`: Read-only queries
- `Aggregate`: Transactional commands with event emission

### 3. Interpreters
- `runProjectionInMemory`: Connects Projection effect to ForgetfulInMemory backend
- `runAggregateInMemory`: Handles transactional updates with locking

## Implementation Status

✅ **Completed:**
- Core effects (Aggregate, Projection)
- In-memory interpreters
- Simple counter example
- Hierarchical model example
- Type parameter helpers
- Basic test suite

## Type Parameter Simplification

The current implementation uses 3 type parameters for effects:
```haskell
Aggregate model event index
Projection model event index
```

### Potential Improvements:

1. **Type Families Approach:**
```haskell
class EventSourced model where
  type Event model
  type Index model
```

2. **Functional Dependencies:**
```haskell
class EventSourced model event index | model -> event index
```

3. **Helper functions** (implemented):
- `withAggregate` for NoIndex aggregates
- `queryModel` for simple projections
- `simpleCommand` for basic commands

## Examples

### Simple Counter
Shows basic usage with NoIndex aggregates and standard Servant API.

### Hierarchical Model
Demonstrates composition with sub-models and complex event hierarchies.

### With Helpers
Uses simplified API with helper functions for common patterns.

## Testing

Run tests with:
```bash
stack test domaindriven-effectful
```

## Next Steps

1. **Postgres Interpreter**: Implement `runAggregatePostgres` and `runProjectionPostgres`
2. **Performance Benchmarks**: Compare with original implementation
3. **Migration Guide**: Document path from GADT-based to Effectful approach
4. **Error Handling**: Add proper error effects and validation
5. **Streaming Support**: Add effect operations for event streaming