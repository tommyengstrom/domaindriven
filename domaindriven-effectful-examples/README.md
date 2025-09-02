# DomainDriven Effectful Examples

Example applications demonstrating the Effectful-based domaindriven library.

## Examples

### Simple Counter
Basic counter application using Effectful effects with standard Servant API.

Run with:
```bash
stack run simple
```

### Simple with Helpers
Counter application using simplified helper functions to reduce boilerplate.

Run with:
```bash
stack run simple-with-helpers
```

### Hierarchical
Complex example with hierarchical models and sub-model composition.

Run with:
```bash
stack run hierarchical
```

## Features Demonstrated

- Using standard Servant combinators (`Get`, `Post`) instead of custom ones
- Effectful effects for domain logic
- In-memory persistence backend
- Type-safe effect composition
- Helper functions for common patterns