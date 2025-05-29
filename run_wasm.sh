#!/usr/bin/env bash

# Usage:   ./run_wasm.sh <args> < [wasm-file]

wasmtime run -W tail-call=y,multi-memory=y --invoke main - $@
