{lib, ...} @ args: let
  modules = {
    nixos-modules = map lib.dot.relativeToRoot [
      # common
      "modules/base.nix"
      # host specific
    ];
    home-modules = map lib.dot.relativeToRoot [
      "home-manager/base.nix"
      # host specific
    ];
  };

  systemArgs = modules // args;
  cfg = lib.dot.nixosSystem systemArgs;
in {
  # macOS's configuration
  nixosConfigurations.nix-sakamoto = cfg;
  # packages.default = cfg.config.system;
}
