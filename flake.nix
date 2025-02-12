{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };
  };

  outputs = {
    nixpkgs,
    flake-utils,
    rust-overlay,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      overlays = [(import rust-overlay)];
      pkgs = import nixpkgs {
        inherit system overlays;
      };
      toolchain = (pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml).override {
        extensions = [
          "rust-src"
        ];
      };
      # nixpkgs.legacyPackages.${system};
    in
      with pkgs; {
        devShells.default = pkgs.mkShell {
          nativeBuildInputs = [
            # Rust stuff
            rust-analyzer-unwrapped
            cargo-flamegraph
            # Nix stuff
            pkgs.nixd
            pkgs.nil
            # Wasm
            pkgs.wasmtime
          ];
          buildInputs = [
            toolchain
          ];
          RUST_SRC_PATH = "${toolchain}/lib/rustlib/src/rust/library";
        };
      });
}
