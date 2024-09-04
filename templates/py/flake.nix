{
  description = "Poetry Python Project";

  inputs = {
    nixpkgs.url = "nixpkgs";
  };

  outputs = {nixpkgs, ...}: let
    inherit (nixpkgs.lib) genAttrs;
    systems = ["x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin"];
    forAllSystems = genAttrs systems;
  in
    forAllSystems (system: let
      pkgs = nixpkgs.legacyPackages.${system};
      pythonPackages = pkgs.python310Packages;
    in {
      devShells.${system} = {
        default = pkgs.mkShell {
          venvDir = "./.venv";
          buildInputs = [
            pythonPackages.venvShellHook
          ];
          packages = [pkgs.poetry];
          postVenvCreation = ''
            unset SOURCE_DATE_EPOCH
            pip install --upgrade pip
            poetry install
          '';
        };
        tools = pkgs.mkShell {
          packages = [pkgs.poetry];
        };
      };
    });
}
