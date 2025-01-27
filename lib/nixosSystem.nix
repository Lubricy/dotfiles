{
  lib,
  inputs,
}: {
  vars,
  nixos-modules,
  home-modules ? [],
  specialArgs ? {},
  ...
}: let
  inherit (inputs) home-manager nixos-generators nix-index-database;
  inherit (vars) system;
in
  lib.nixosSystem {
    inherit system;
    specialArgs = {inherit lib system;} // inputs // specialArgs;
    modules =
      [
        ../modules/nixos
        ../overlays
        nixos-generators.nixosModules.all-formats
        nix-index-database.nixosModules.nix-index
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
            home-manager.backupFileExtension = "bak";
            home-manager.extraSpecialArgs = inputs // specialArgs;
          })
          ({
            lib,
            config,
            ...
          }: let
            cfg = config.dot.defaultUser;
          in {
            config = lib.mkIf cfg.enable {
              home-manager.users."${cfg.username}".imports = home-modules ++ [{inherit (config) dot;}];
            };
          })
        ]
      );
  }
