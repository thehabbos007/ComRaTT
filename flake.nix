{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-darwin" "aarch64-darwin" "x86_64-linux" ];
      createDevShell = system:
        let
          pkgs = import nixpkgs { inherit system; };
          inferno-version = "20220603";
          inferno = pkgs.ocamlPackages.buildDunePackage {
            pname = "inferno";
            version = "v${inferno-version}";
            src = pkgs.fetchFromGitLab {
              domain = "gitlab.inria.fr";
              owner = "fpottier";
              repo = "inferno";
              rev = "${inferno-version}";
              sha256 = "sha256-Cq08GbKnyK0CIbj5NQ6RpU3pzrycWbfEj//lde0kYNY=";
            };

            propagatedBuildInputs = [ pkgs.ocamlPackages.pprint pkgs.ocamlPackages.unionFind ];
          };
        in
        pkgs.mkShell {
          buildInputs = [
            # Nix stuff
            pkgs.nixd
            pkgs.nil
            # Wasm
	          pkgs.wasmtime
						# Ocaml with build system
            pkgs.ocaml
            pkgs.ocamlPackages.findlib
            pkgs.dune_3
            pkgs.ocamlPackages.ocaml-lsp
            pkgs.ocamlformat
            pkgs.ocamlPackages.ocamlformat-rpc-lib
            pkgs.ocamlPackages.utop

            # Our actual pacakge dependencies
            pkgs.ocamlPackages.ppx_deriving
            pkgs.ocamlPackages.ppx_expect
            pkgs.ocamlPackages.ppx_inline_test
            pkgs.ocamlPackages.menhir
            pkgs.ocamlPackages.menhirLib
            pkgs.ocamlPackages.base
            pkgs.ocamlPackages.asai
            pkgs.ocamlPackages.ounit2
            inferno
          ];
        };
    in
    {
      devShell = nixpkgs.lib.genAttrs systems createDevShell;
    };
}
