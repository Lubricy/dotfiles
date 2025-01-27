{
  lib,
  config,
  ...
}: {
  home.homeDirectory = "/Users/${config.home.username}";

  # modules.tools.open-interpreter.enable = true;
  imports =
    (lib.dot.scanPaths ./.)
    ++ [
      ../base
    ];
}
