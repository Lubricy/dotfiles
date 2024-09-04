{
  lib,
  config,
  ...
}: {
  home.homeDirectory = "/Users/${config.vars.username}";

  # modules.tools.open-interpreter.enable = true;
  imports =
    (lib.dot.scanPaths ./.)
    ++ [
      ../base
    ];
}
