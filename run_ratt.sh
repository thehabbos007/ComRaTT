#!/usr/bin/env bash

# Usage:   ./run_ratt.sh <args> < [comratt-file]

dune exec -- ComRaTT | wasmtime run -W tail-call=y,multi-memory=y --invoke main - $@
