{
  lib,
  inputs,
}: {
  vars,
  darwin-modules,
  home-modules ? [],
  ...
}: let
  inherit (inputs) nixpkgs-darwin home-manager nix-darwin;
  inherit (vars) system;
in
  nix-darwin.lib.darwinSystem {
    specialArgs = {inherit lib system;} // inputs;
    modules =
      [
        ../modules/darwin
        ./overlays.nix
        {
          inherit vars;
          nixpkgs.pkgs = import nixpkgs-darwin {inherit system;};
          imports = darwin-modules;
        }
      ]
      ++ (
        lib.optionals ((lib.lists.length home-modules) > 0)
        [
          home-manager.darwinModules.home-manager
          ({config, ...}: {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;

            home-manager.backupFileExtension = "bak";
            home-manager.extraSpecialArgs = inputs;
            home-manager.users."${config.vars.username}".imports = home-modules ++ [{inherit vars;}];
          })
        ]
      );
  }
