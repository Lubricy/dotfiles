{lib, ...} @ args: let
  modules = {
    darwin-modules = map lib.dot.relativeToRoot [
      # common
      "modules/darwin"
      # host specific
    ];
    home-modules = map lib.dot.relativeToRoot [
      "home-manager/darwin"
      # host specific
    ];
  };

  systemArgs = modules // args;
  cfg = lib.dot.macosSystem systemArgs;
in {
  # darwinConfigurations.default = import (lib.dot.relativeToRoot "modules/darwin");
  darwinConfigurations.default = cfg;
  packages.default = cfg.system;
}
