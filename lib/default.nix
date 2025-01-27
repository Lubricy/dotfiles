{lib, ...} @ args:
{
  relativeToRoot = lib.path.append ../.;
  linkRepo = import ./linkRepo.nix lib;
  linkShared = config: names:
    lib.genAttrs names (dir: {
      source = config.lib.file.mkOutOfStoreSymlink "${config.xdg.configHome}/repos/dotfiles/shared/${dir}";
    });
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
// import ./mkSystem.nix args
