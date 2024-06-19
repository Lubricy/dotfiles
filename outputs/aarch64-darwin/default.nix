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
      ++ myvars.darwin-modules;
    home-modules = map mylib.relativeToRoot [
      "home-manager/darwin"
      # host specific
    ] ++ myvars.home-modules;
  };

  systemArgs = modules // args;
in {
  # macOS's configuration
  darwinConfigurations.${name} = mylib.macosSystem systemArgs;
  # packages.default = inputs.nix-darwin.packages.${system}.default;
}

