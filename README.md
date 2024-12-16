# Dependencies
## With Nix
If you use nix, `nix develop` or nix-direnv is enough to install all dependencies.

## Docker

The provided Dockerfile creates an image with everything in place.

Build the image with

`docker build -t comratt .`

## Bare metal
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
# Returns number of steps to find x in 0..100
$ ./run_ratt.sh 51 < examples/bin_search.cml
$ ./run_ratt.sh < examples/frp.cml
```

This should compile the ComRaTT snippets to WAT and pipe the WAT code into wasmtime.
Any arguments passed into the shell script is passed directly as parameters to the exported `main` functions
of the ComRaTT programs.

## With Docker
Use the following command to execute examples in the container directly from your host shell.

`docker run --rm comratt /bin/bash -c "./run_ratt.sh 1 < examples/is_prime.cml"` 

Replace examples and arguments according to the instructions above.

It is also possible to use `docker run -it comratt` to enter the containers shell and interact with it manually.
