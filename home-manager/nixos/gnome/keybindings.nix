# Generated via dconf2nix: https://github.com/gvolpe/dconf2nix
{lib, ...}: let
  inherit
    (lib)
    pipe
    flip
    range
    genAttrs
    foldAttrs
    concatMapAttrs
    ;
  num-workspaces = 9;
  attrRange = s: e:
    pipe (range s e) [
      (map toString)
      ((flip genAttrs) (x: [x "KP_${x}"]))
    ];
  attrCount = attrRange 1;
  workspaces = attrCount num-workspaces;

  directions = {
    left = ["Left" "KP_Left" "h"];
    right = ["Right" "KP_Right" "l"];
    up = ["Up" "KP_Up" "k"];
    down = ["Down" "KP_Down" "j"];
  };
  key-bindings' = reducer: empty: bind-fn:
    concatMapAttrs (k: vs: let
      ks = bind-fn k; # ks => v: { ~k = [~v]; }
      vs' = map ks vs; # vs' => [ { ~k = [~v]; } ]
    in
      foldAttrs reducer empty vs');
  key-bindings = flip (key-bindings' (l: r: l ++ r) []);
in {
  dconf.settings = {
    "org/gnome/desktop/wm/preferences" = {
      inherit num-workspaces;
    };

    "org/gnome/desktop/wm/keybindings" =
      {
        activate-window-menu = ["<Super>c"];
        begin-move = [];
        begin-resize = [];
        close = ["<Super>x" "<Alt>F4"];
        maximize = [];
        minimize = [];
        move-to-monitor-left = [];
        move-to-monitor-right = [];
        panel-run-dialog = [];
        switch-to-workspace-left = ["<Ctrl><Super>Left"];
        switch-to-workspace-right = ["<Ctrl><Super>Right"];
        switch-to-workspace-up = ["<Ctrl><Super>Up"];
        switch-to-workspace-down = ["<Ctrl><Super>Down"];
        toggle-maximized = [];
        unmaximize = [];
      }
      // (key-bindings (workspaces // directions)
        (n: v: {
          "move-to-workspace-${n}" = ["<Shift><Super>${v}"];
          "switch-to-workspace-${n}" = ["<Super>${v}"];
        }));

    "org/gnome/mutter/keybindings" = {
      toggle-tiled-left = [];
      toggle-tiled-right = [];
    };

    "org/gnome/mutter/wayland/keybindings" = {
      restore-shortcuts = [];
    };

    "org/gnome/settings-daemon/plugins/media-keys" = {
      help = [];
      logout = ["<Super>q"];
      magnifier = [];
      magnifier-zoom-in = ["<Super>equal"];
      magnifier-zoom-out = ["<Super>minus"];
      screenreader = [];
      screensaver = [];
    };

    "org/gnome/shell/keybindings" =
      {
        focus-active-notification = [];
        screenshot = ["<Shift><Alt>4"];
        show-screen-recording-ui = [];
        show-screenshot-ui = ["<Shift><Alt>3"];
        toggle-application-view = ["<Super>t"];
        toggle-message-tray = ["<Shift><Super>n"];
        toggle-overview = [];
        toggle-quick-settings = ["<Shift><Super>c"];
      }
      // (key-bindings (attrCount 4)
        (n: _: {
          "switch-to-application-${n}" = [];
        }));

    "org/gnome/shell/extensions/pop-shell" =
      {
        tile-orientation = ["<Super>r"];
        tile-enter = ["<Super>w"];
        management-orientation = ["r"];
        # (un)float a window
        toggle-floating = ["<Super>f"];
        # (un)float all windows
        toggle-tiling = ["<Shift><Super>f"];
      }
      // (key-bindings directions
        (d: v: {
          # Move window {direction}
          "tile-move-${d}" = [v];
          # Resize window {direction}
          "tile-resize-${d}" = ["<Control>${v}"];
          # Swap window {direction}
          "tile-swap-${d}" = ["<Shift>${v}"];

          # Move window to the workspace at {direction}
          "pop-workspace-${d}" = ["<Shift><Super>${v}"];
          # Move window to the monitor at {direction}
          "pop-monitor-${d}" = ["<Control><Super>${v}"];
        }));
  };
}
