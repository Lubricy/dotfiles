{
  inputs,
  lib,
  mylib,
  myvars,
  system,
  genSpecialArgs,
  ...
} @ args: let
  name = myvars.hostname;

  modules = {
    nixos-modules =
      (map mylib.relativeToRoot [
        # common
        "modules/base.nix"
        # host specific
      ])
      ++ (myvars.nixos-modules or []);
    home-modules = map mylib.relativeToRoot [
      "home-manager/base.nix"
      # host specific
    ] ++ (myvars.home-modules or []);
  };

  systemArgs = modules // args;
  cfg = mylib.nixosSystem systemArgs;
in {
  # macOS's configuration
  nixosConfigurations.${name} = cfg;
  # packages.default = cfg.config.system;
}
