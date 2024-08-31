{
  lib,
  inputs,
}: {
  # https://github.com/nix-community/home-manager/blob/master/modules/lib/stdlib-extended.nix
  macosSystem = import ./macosSystem.nix {inherit lib inputs;};
  nixosSystem = import ./nixosSystem.nix {inherit lib inputs;};

  attrs = import ./attrs.nix {inherit lib;};
  linkRepo = import ./linkRepo.nix {inherit lib;};

  # use path relative to the root of the project
  relativeToRoot = lib.path.append ../.;
  fromShared = lib.path.append ../shared/.;
  scanPaths = path:
    builtins.map
    (f: (path + "/${f}"))
    (builtins.attrNames
      (lib.attrsets.filterAttrs
        (
          path: _type:
            (_type == "directory") # include directories
            || (
              (path != "default.nix") # ignore default.nix
              && (lib.strings.hasSuffix ".nix" path) # include .nix files
            )
        )
        (builtins.readDir path)));
}
