#!/usr/bin/env bash

# Usage:   ./run_ratt.sh [comratt-file] <args>

dune exec -- ComRaTT $1 | wasmtime run -W tail-call=y,multi-memory=y --invoke main - ${@:2}
