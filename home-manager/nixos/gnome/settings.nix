{
  lib,
  pkgs,
  ...
}: let
  extensions = with pkgs.gnomeExtensions; [
    pop-shell
  ];
  inherit (lib.hm.gvariant) mkUint32;
in {
  home.packages = extensions;
  dconf.settings = {
    "org/gnome/shell/extensions/pop-shell" = {
      active-hint = false;
      mouse-cursor-focus-location = mkUint32 4;
      show-skip-taskbar = true;
      show-title = true;
      smart-gaps = true;
      stacking-with-mouse = false;
      tile-by-default = true;
    };

    "org/gnome/shell" = {
      disable-user-extensions = false;
      enabled-extensions =
        map (p: p.extensionUuid) extensions
        ++ [
          "system-monitor@gnome-shell-extensions.gcampax.github.com"
        ];
    };

    "org/gnome/settings-daemon/plugins/power" = {
      idle-dim = false;
      sleep-inactive-ac-type = "nothing";
    };

    "org/gnome/settings-daemon/plugins/color" = {
      night-light-schedule-automatic = false;
    };

    "org/gnome/mutter" = {
      dynamic-workspaces = false;
      edge-tiling = false;
      workspaces-only-on-primary = true;
    };

    "org/gnome/desktop/peripherals/touchpad" = {
      disable-while-typing = true;
      two-finger-scrolling-enabled = true;
    };

    "org/gnome/desktop/interface" = {
      color-scheme = "prefer-dark";
      cursor-size = 32;
      cursor-theme = "capitaine-cursors";
      font-name = "Sans 11";
      gtk-theme = "Adwaita-dark";
      icon-theme = "Adwaita";
      show-battery-percentage = true;
    };

    "org/gnome/tweaks" = {
      show-extensions-notice = false;
    };
  };
}
