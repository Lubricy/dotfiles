{
  lib,
  config,
  ...
}: {
  home.homeDirectory = "/home/${config.home.username}";
  programs.alacritty.settings.font.size = lib.mkOverride 900 12;
  imports =
    (lib.dot.scanPaths ./.)
    ++ [
      ../base
    ];
}
