{
  lib,
  config,
  pkgs,
  ...
}: let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.notifications.mako;
in {
  options.modules.notifications.mako = {
    enable = mkEnableOption "mako notification service";
  };
  config = mkIf cfg.enable {
    home.packages = [pkgs.mako];
    xdg.configFile = lib.dot.linkShared config ["mako"];
  };
}
