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
    darwin-modules =
      (map mylib.relativeToRoot [
        # common
        "modules/darwin"
        # host specific
      ])
      ++ (myvars.darwin-modules or []);
    home-modules = map mylib.relativeToRoot [
      "home-manager/darwin"
      # host specific
    ] ++ (myvars.home-modules or []);
  };

  systemArgs = modules // args;
  cfg = mylib.macosSystem systemArgs;
in {
  # macOS's configuration
  darwinConfigurations.${name} = cfg;
  packages.default = cfg.system;
}
