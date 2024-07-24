{
  description = "Basic Rust Project";

  inputs.nixpkgs.url = "nixpkgs";
  inputs.flake-parts.url = "github:hercules-ci/flake-parts";
  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      flake = {
        # Put your original flake attributes here.
      };
      systems = [
        # systems for which you want to build the `perSystem` attributes
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
        "x86_64-darwin"
      ];
      perSystem = { config, pkgs, system, ... }:
        let
          isDarwin = pkgs.lib.strings.hasSuffix "-darwin" system;
          darwinOptions = pkgs.lib.optionalAttrs isDarwin {
            buildInputs = [
              pkgs.darwin.apple_sdk.frameworks.Foundation
            ];
          };
        in
          {
            devShells.tools = with pkgs; mkShell {
              buildInputs = [ cargo rustc ];
            };
            packages.default = pkgs.rustPlatform.buildRustPackage ({
              name = "rust-app";
              src = pkgs.lib.cleanSource ./.;
              cargoLock  = {
                lockFile = ./Cargo.lock;
                allowBuiltinFetchGit = true;
              };
            } // darwinOptions);
          };
    };
}
