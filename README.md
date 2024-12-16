# Dependencies
If you use nix, `nix develop` or nix-direnv is enough to install all dependencies.

Otherwise you need
- OCaml (tested with version 5.1.1~rc1)
- opam 
- wasmtime (https://github.com/bytecodealliance/wasmtime) (tested with version 26.0.0)

Install the projects dependencies via
```opam install . --deps-only```

# Running

`dune exec -- ComRaTT [args]`
