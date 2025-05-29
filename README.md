# ComRaTT - Compiled Async RaTT

ComRaTT is a functional reactive programming language that compiles to WebAssembly. This implementation is part of a thesis project demonstrating the feasibility of compiling Async RaTT to WASM with support for modal types and reactive semantics.

## Dependencies

### With Nix (easy)
If you use nix, `nix develop` or nix-direnv (`direnv allow`) is enough to install all dependencies.

### Manual Installation
All you need is rust through rustup (https://www.rust-lang.org/tools/install). The rust toolchain version is set through the `rust-toolchain.toml` file automatically.

## Building the Compiler

Build the ComRaTT compiler with:
```bash
cargo build
```

## Running Example Programs

We provide several example programs that demonstrate different features of ComRaTT.

```bash
# Factorial calculation (argument: n)
cargo run -- examples/factorial.cml 5

# Fibonacci sequence (argument: n)
cargo run -- examples/fib.cml 10

# Prime number check (argument: n)
cargo run -- examples/is_prime.cml 17

# Collatz conjecture (argument: n)
cargo run -- examples/collatz.cml 7

# Binary search (argument: n, number to search for in range 0..100)
cargo run -- examples/bin_search.cml 42
```

### Reactive Programming Examples

To enable the runtime, use the `--run` CLI flag

```bash
# Signal recursion example (interactive keyboard input)
cargo run -- --run examples/sigrec.cml

# Keyboard input once, and then never activate again
cargo run -- --run examples/42never.cml
```

## Example Program Structure

A typical ComRaTT program looks like:
```comratt
// Function definition
factorial : int -> int
def factorial n =
  if n <= 1
  then 1
  else n * (factorial (n - 1));

// Main function (entry point)
main : int -> int
def main x = factorial x;
```

For reactive programs:
```comratt
// Channel declaration
chan keyboard : int;

// Signal definition
signal : O Sig int
def signal =
  let x = wait keyboard in
  delay {cl(x)} (
    let val = advance x in
    val :: signal
  );

// Output connection
print <- signal;
```
