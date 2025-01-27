{
  lib,
  inputs,
}: let
  inherit
    (inputs)
    nixpkgs
    nix-darwin
    nix-on-droid
    nixos-generators
    nix-index-database
    home-manager
    ;
  inherit (lib) path optionals length;
  dotfileInputs = inputs;
  mkSystem = {
    fn,
    name,
    modules ? [],
    pkgs ? nixpkgs,
  }: {
    inputs,
    pkgs ? pkgs.legacyPackages."${builtins.currentSystem}",
    modules ? [],
    home-modules ? [],
    specialArgs ? {},
  }: let
    inherit (inputs) self;
  in
    fn {
      specialArgs = {inherit lib;} // dotfileInputs // inputs // specialArgs;
      modules =
        [
          (path.append ../modules name)
          ../overlays
          nix-index-database.${"${name}Modules"}.nix-index
          {
            nixpkgs.pkgs = lib.mkDefault pkgs;
            system.configurationRevision =
              self.shortRev
              or self.dirtyShortRev
              or self.lastModified
              or "unknown";
          }
        ]
        ++ modules
        ++ optionals ((length home-modules) > 0)
        [
          home-manager.${"${name}Modules"}.home-manager
          ({
            lib,
            config,
            ...
          }: {
            config = lib.mkIf config.dot.defaultUser.enable {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.backupFileExtension = "bak";
              home-manager.extraSpecialArgs = inputs // specialArgs;
              home-manager.users."${config.dot.defaultUser.username}".imports =
                home-modules
                ++ [
                  (args: {
                    home.username = args."${name}Config".dot.defaultUser.username;
                  })
                ];
            };
          })
        ];
    };
in {
  nixosSystem = mkSystem {
    fn = lib.nixosSystem;
    name = "nixos";
    modules = [
      nixos-generators.nixosModules.all-formats
    ];
  };
  macosSystem = mkSystem {
    name = "darwin";
    fn = nix-darwin.lib.darwinSystem;
    pkgs = nix-darwin;
  };
  droidSystem = mkSystem {
    name = "nixos";
    fn = {
      pkgs,
      modules,
      specialArgs ? {},
      ...
    }:
      nix-on-droid.lib.nixOnDroidConfiguration {
        inherit pkgs modules;
        extraSpecialArgs = specialArgs;
      };
    modules = [
      nixos-generators.nixosModules.all-formats
    ];
  };
}
