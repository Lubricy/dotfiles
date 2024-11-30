{
  lib,
  pkgs,
  ...
}: {
  imports = lib.dot.scanPaths ./.;
  home.packages = [pkgs.libnotify];
  modules.notifications.mako.enable = lib.mkDefault true;
}
