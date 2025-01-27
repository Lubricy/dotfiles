{
  lib,
  pkgs,
  config,
  ...
}: {
  imports = lib.dot.scanPaths ./.;
  options.features.wm.notification.enable = lib.mkEnableOption "Notification";
  config = lib.mkIf config.features.wm.notification.enable {
    home.packages = [pkgs.libnotify];
    modules.notifications.mako.enable = lib.mkDefault true;
  };
}
