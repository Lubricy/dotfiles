{
  lib,
  pkgs,
  ...
}: {
  imports = lib.dot.scanPaths ./.;
  wayland.windowManager.hyprland = {
    # Whether to enable Hyprland wayland compositor
    enable = true;
    # The hyprland package to use
    package = pkgs.hyprland;
    # Whether to enable XWayland
    xwayland.enable = true;

    # Optional
    # Whether to enable hyprland-session.target on hyprland startup
    systemd.enable = true;
    # settings
    settings = {
      "$mod" = "ALT";
      monitor = [
        # needed because nvidia driver deserves a middle finger
        "Unknown-1,disable"
      ];
      exec-once = [
        "eww open bar"
      ];
      "debug:disable_logs" = false;
    };
  };
}