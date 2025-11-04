#!/usr/bin/env bash

# Exit on error
set -x
set -e

# Configuration
LOG_FILE="ghcid.log"

echo "Starting ghciwatch"
echo "Logging to: $LOG_FILE"
echo "----------------------------------------"

# Run ghciwatch with:
# - hpack before each startup/restart to regenerate .cabal files
# - cabal repl with the specific test-dev component
# - output redirected to log file
#
#
components=$(gen-hie |grep component|grep '".*"' -o|sed "s:\"::g" | xargs echo)

echo cabal v2-repl --enable-multi-repl $components

ghcid \
  --command "cabal v2-repl --enable-multi-repl $components" \
  --restart "cabal.project" \
  --restart "streaker.cabal" \
  --restart "chatcompletion-effectful.cabal" \
  -o $LOG_FILE
