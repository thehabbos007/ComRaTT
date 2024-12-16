# Dependencies
If you use nix, `nix develop` or nix-direnv is enough to install all dependencies.

Otherwise you need
- OCaml (tested with version 5.1.1~rc1)
- opam
- wasmtime (https://github.com/bytecodealliance/wasmtime) (tested with version 26.0.0)

Install the projects dependencies via
```opam install . --deps-only```

# Running

We have a directory of example programs. These can be run with the help of wasmtime and small bash script

```terminal
$ chmod +x run_ratt.sh
$ ./run_ratt.sh 1 < examples/is_prime.cml
$ ./run_ratt.sh 3 < examples/collatz.cml
$ ./run_ratt.sh 3 < examples/factorial.cml
$ ./run_ratt.sh 7 < examples/fib.cml
$ ./run_ratt.sh < examples/frp.cml
```

This should compile the ComRaTT snippets to WAT and pipe the WAT code into wasmtime.
Any arguments passed into the shell script is passed directly as parameters to the exported `main` functions
of the ComRaTT programs.
