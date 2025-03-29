{
  lib,
  config,
  pkgs,
  ...
}: {
  imports = lib.dot.scanPaths ./.;
  config = lib.mkIf config.features.wm.enable {
    wayland.windowManager.hyprland = {
      # Optional
      # Whether to enable hyprland-session.target on hyprland startup
      systemd.enable = false;
      # systemd.extraCommands =
      #   [
      #     "bash -l -c 'dbus-update-activation-environment --systemd --all'"
      #     "systemctl --user start eww"
      #   ]
      #   ++ options.wayland.windowManager.hyprland.systemd.extraCommands.default;
      # Whether to enable Hyprland wayland compositor
      enable = true;
      # The hyprland package to use
      package = pkgs.hyprland;
      # Whether to enable XWayland
      xwayland.enable = true;

      # settings
      settings = {
        "$mod" = "ALT";
        # monitor = [
        #   # needed because nvidia driver deserves a middle finger
        #   "Unknown-1,disable"
        # ];
        exec-once = [
          "swww-daemon"
          "eww open bar"
        ];
        "debug:disable_logs" = false;
      };
    };
  };
}
