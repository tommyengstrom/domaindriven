# AGENTS.md

## Development Workflow

Use `process-compose` for the normal build, test, benchmark, and live-feedback workflow in this repository.

Before starting a new `process-compose` session, always try to attach to an already running session:

```bash
process-compose attach
```

Only start a new session when attaching fails because no session is running:

```bash
process-compose up
```

The configured process graph lives in `process-compose.yaml`. It includes dependency setup, full builds, tests, benchmarks, and `ghcid` live feedback.

Useful checks:

```bash
process-compose -f process-compose.yaml --dry-run
process-compose graph
process-compose process list
```
