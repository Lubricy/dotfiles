{config, ...}: {
  wayland.windowManager.hyprland.settings = {
    source = "${config.xdg.configHome}/hypr/colors.conf";
    decoration = {
      shadow_offset = "5 5";
      "col.shadow" = "$shadow";
      rounding = 10;
      inactive_opacity = 0.9;
      dim_inactive = true;
      dim_strength = 0.3;
    };

    general = {
      border_size = 2;
      "col.active_border" = "$primary";
      "col.inactive_border" = "$on_primary";
      resize_on_border = true;
    };

    misc = {
      disable_splash_rendering = true;
      disable_hyprland_logo = true;
      force_default_wallpaper = 0; # Set to 0 to disable the anime mascot wallpapers
    };
  };
}
