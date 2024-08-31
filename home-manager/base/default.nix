{lib, ...}: {
  imports = [(lib.dot.relativeToRoot "modules/vars.nix")] ++ (lib.dot.scanPaths ./.);
}
