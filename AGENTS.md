# AGENTS.md

## Development Workflow

Use `process-compose` for the normal build, test, benchmark, and live-feedback workflow in this repository.

Check whether `process-compose` is already running before starting work:

```bash
process-compose list -owide
```

When the session needs to be restarted, bring it down; it will restart automatically:

```bash
process-compose down
```

The configured process graph lives in `process-compose.yaml`. It includes dependency setup, full builds, tests, benchmarks, and `ghcid` live feedback.

Prefer relying on `ghcid` for compiler feedback.

Useful checks:

```bash
process-compose -f process-compose.yaml --dry-run
process-compose graph
process-compose process list
```
