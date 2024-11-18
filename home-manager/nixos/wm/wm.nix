{
  lib,
  config,
  pkgs,
  ...
}: {
  xdg.configFile = lib.dot.linkShared config ["eww" "matugen"];
  dconf.settings = {
    "org/gnome/desktop/background" = {
      picture-uri-dark = "file://${pkgs.nixos-artwork.wallpapers.nineish-dark-gray.src}";
    };
    "org/gnome/desktop/interface" = {
      color-scheme = "prefer-dark";
    };
  };
  gtk = {
    enable = true;
    theme = {
      name = "Adwaita-dark";
      package = pkgs.gnome.gnome-themes-extra;
    };
  };

  qt = {
    enable = true;
    platformTheme.name = "adwaita";
    style.name = "adwaita-dark";
  };

  home.packages = with pkgs; [
    eww
    matugen
  ];

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
    settings = let
      inherit (lib) range imap length concatMap;
    in {
      "$mod" = "ALT";
      workspace = [
        "1, persistent:true, defaultName:Terminal"
        "2, persistent:true, defaultName:Browser"
        "3, persistent:true, defaultName:Editor"
      ];
      monitor = [
        # needed because nvidia driver deserves a middle finger
        "Unknown-1,disable"
      ];
      windowrulev2 = let
        inherit (lib) mapAttrsToList flatten concatStringsSep;
        attrs = mapAttrsToList (k: v: "${k}:${v}");
        line = window: action: concatStringsSep ", " ([action] ++ attrs window);
        lines = _: v: map (line v.window) v.actions;
        rules = {
          capture = {
            window.title = "^doom-capture$";
            actions = [
              "float"
              "size 35% 50%"
              "minsize 500 400"
              "center 1"
              "pin"
              "stayfocused"
            ];
          };
          launcher = {
            window.title = "^Launcher$";
            actions = [
              "float"
              "size 35% 50%"
              "minsize 500 400"
              "center 1"
              "pin"
              "stayfocused"
            ];
          };
          emacs = {
            window.class = "^emacs$";
            window.title = "^(?!org-capture)$";
            actions = [
              "workspace name:Editor"
            ];
          };
          firefox = {
            window.class = "^firefox$";
            actions = [
              "workspace name:Browser"
            ];
          };
        };
      in
        flatten (mapAttrsToList lines rules);
      exec-once = [
        "eww open bar"
      ];
      "debug:disable_logs" = false;
      decoration = {
        shadow_offset = "0 5";
        "col.shadow" = "rgba(00000099)";
      };
      bind =
        [
          "$mod,P,exec,alacritty"
          "$mod,Q,exec,asztal -t powermenu"
          "$mod,W,exec,asztal -t overview"
          "$mod,N,exec,~/.config/emacs/bin/org-capture"
          "$mod,X,killactive"
          "$mod SHIFT,Q,exit"
        ]
        ++ (concatMap (i: [
          "$mod,${toString i},workspace,${toString i}"
          "$mod SHIFT,${toString i},movetoworkspace,${toString i}"
        ]) (range 1 9));
      bindm = [
        # mouse movements
        "$mod, mouse:272, movewindow"
        "$mod, mouse:273, resizewindow"
        "$mod SHIFT, mouse:272, resizewindow"
      ];
      misc = {
        disable_splash_rendering = true;
        disable_hyprland_logo = true;
        force_default_wallpaper = 0; # Set to 0 to disable the anime mascot wallpapers
      };
    };
  };
}
