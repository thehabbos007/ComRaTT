#!/usr/bin/env bash

# Usage:   ./run.sh <args> < [wasm-file]
# or pipe: dune exec -- comptest | ./run_wasm.sh

wasmtime run -W tail-call=y,multi-memory=y,all-proposals=y --invoke main - $@
