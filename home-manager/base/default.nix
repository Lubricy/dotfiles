{lib, ...}: {
  imports =
    [
      (lib.dot.relativeToRoot "modules/common/vars.nix")
    ]
    ++ (lib.dot.scanPaths ./.);
}
