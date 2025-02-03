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
        in
        pkgs.mkShell {
          buildInputs = [
            pkgs.ocaml
            pkgs.ocamlPackages.findlib
            pkgs.dune_3
            pkgs.ocamlPackages.ocaml-lsp
            pkgs.ocamlformat
            pkgs.ocamlPackages.ocamlformat-rpc-lib
            pkgs.ocamlPackages.utop

            pkgs.ocamlPackages.ppx_assert
            pkgs.ocamlPackages.ppx_deriving
            pkgs.ocamlPackages.ppx_inline_test
            pkgs.ocamlPackages.menhir
            pkgs.ocamlPackages.base
            pkgs.ocamlPackages.ounit2
          ];
        };
    in
    {
      devShell = nixpkgs.lib.genAttrs systems createDevShell;
    };
}
