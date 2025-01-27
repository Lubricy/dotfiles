{
  lib,
  config,
  pkgs,
  ...
}: {
  config = lib.mkIf config.features.wm.enable {
    home.packages = with pkgs; [
      eww
      swww
      unstable.matugen
    ];
    xdg.configFile = lib.dot.linkShared config ["eww" "matugen"];

    programs.hyprlock.enable = true;
    services.hypridle = {
      enable = true;
      settings = {
        general = {
          after_sleep_cmd = "hyprctl dispatch dpms on";
          ignore_dbus_inhibit = false;
        };

        listener = [
          {
            timeout = 1200;
            on-timeout = "hyprctl dispatch dpms off";
            on-resume = "hyprctl dispatch dpms on";
          }
        ];
      };
    };
  };
}
