#!/usr/bin/env bash

# Usage:   ./run_wasm.sh <args> < [wasm-file]
# or pipe: dune exec -- comptest | ./run_wasm.sh <args>

wasmtime run -W tail-call=y,multi-memory=y --invoke main - $@
