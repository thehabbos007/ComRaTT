#!/usr/bin/env bash

# Usage:   ./run_ratt.sh <args> < [comratt-file]

dune exec -- ComRaTT $1 | wasmtime run -W tail-call=y,multi-memory=y --invoke main - ${@:2}
