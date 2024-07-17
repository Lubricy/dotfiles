{
  lib,
  mylib,
  myvars,
  ...
}: {
  home.homeDirectory = "/Users/${myvars.username}";
  # modules.tools.open-interpreter.enable = true;
  imports =
    (mylib.scanPaths ./.)
    ++ [
      ../base/core
      ../base/tui
      ../base/gui
      ../base/home.nix
    ];
}
