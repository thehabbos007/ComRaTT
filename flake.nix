{
  inputs = {
    opam-nix.url = "github:tweag/opam-nix";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.follows = "opam-nix/nixpkgs";
    unstable.url = "nixpkgs/nixos-unstable";
  };
  outputs = {
    self,
    flake-utils,
    opam-nix,
    nixpkgs,
    unstable,
  } @ inputs:
  # Don't forget to put the package name instead of `throw':
  let
    package = "ComRaTT";
  in
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};
      upkgs = unstable.legacyPackages.${system};
      on = opam-nix.lib.${system};
      localPackagesQuery =
        builtins.mapAttrs (_: pkgs.lib.last)
        (on.listRepo (on.makeOpamRepo ./.));
      devPackagesQuery = {
        # You can add "development" packages here. They will get added to the devShell automatically.
        utop = "*";
        ocaml-lsp-server = "*";
        ocamlformat = "*";
        dune = "*";
      };
      query =
        devPackagesQuery
        // {
          ## You can force versions of certain packages here, e.g:
          ## - force the ocaml compiler to be taken from opam-repository:
          ocaml-base-compiler = "*";
          ## - or force the compiler to be taken from nixpkgs and be a certain version:
          # ocaml-system = "4.14.0";
          ## - or force ocamlfind to be a certain version:
          # ocamlfind = "1.9.2";
          ppx_deriving = "*";
          ounit2 = "*";
          menhir = "*";
          base = "*";
        };
      scope = on.buildDuneProject {} "ComRaTT" ./. query;
      overlay = final: prev: {
        # You can add overrides here
        ${package} = prev.${package}.overrideAttrs (_: {
          # Prevent the ocaml dependencies from leaking into dependent environments
          doNixSupport = false;
        });
      };
      scope' = scope.overrideScope' overlay;
      # Packages in this workspace
      packages =
        pkgs.lib.getAttrs (builtins.attrNames localPackagesQuery) scope';
      # Packages from devPackagesQuery
      devPackages =
        builtins.attrValues
        (pkgs.lib.getAttrs (builtins.attrNames devPackagesQuery) scope');
    in {
      legacyPackages = scope';

      packages = packages // {default = packages.ComRaTT;};

      devShells.default = pkgs.mkShell {
        inputsFrom = builtins.attrValues packages;
        buildInputs =
          devPackages
          ++ [
            pkgs.wasmer
            upkgs.wasmtime
    	    pkgs.python311
    	    pkgs.wabt
          ];
      };
    });
}
