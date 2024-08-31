{
  lib,
  inputs,
}: {
  vars,
  # genSpecialArgs,
  nixos-modules,
  home-modules ? [],
  # specialArgs ? (genSpecialArgs system),
  ...
}: let
  inherit (inputs) home-manager nixos-generators;
  inherit (vars) system;
in
  lib.nixosSystem {
    inherit system;
    specialArgs = {inherit lib inputs;};
    modules =
      [
        ../modules/darwin
        ./overlays.nix
        nixos-generators.nixosModules.all-formats
        {
          inherit vars;
          imports = nixos-modules;
        }
      ]
      ++ (
        lib.optionals ((lib.lists.length home-modules) > 0)
        [
          home-manager.nixosModules.home-manager
          ({config, ...}: {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;

            home-manager.extraSpecialArgs = inputs;
            home-manager.users."${config.vars.username}".imports = home-modules ++ [{inherit vars;}];
          })
        ]
      );
  }
