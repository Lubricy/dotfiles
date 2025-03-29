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
  arrow-directions = {
    left = ["Left" "KP_Left"];
    right = ["Right" "KP_Right"];
    up = ["Up" "KP_Up" "k"];
    down = ["Down" "KP_Down"];
  };
  vim-directions = {
    left = ["h"];
    right = ["l"];
    up = ["k"];
    down = ["j"];
  };
  fold-list = l: r: l ++ r;
  directions = foldAttrs fold-list [] [
    vim-directions
    arrow-directions
  ];
  key-bindings' = reducer: empty: bind-fn:
    concatMapAttrs (k: vs: let
      ks = bind-fn k; # ks => v: { ~k = [~v]; }
      vs' = map ks vs; # vs' => [ { ~k = [~v]; } ]
    in
      foldAttrs reducer empty vs');
  key-bindings = flip (key-bindings' fold-list []);
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
        panel-run-dialog = [];
        toggle-maximized = [];
        unmaximize = [];
      }
      // (key-bindings workspaces
        (n: v: {
          "move-to-workspace-${n}" = ["<Shift><Super>${v}"];
          "switch-to-workspace-${n}" = ["<Super>${v}"];
        }))
      // (key-bindings directions
        (n: v: {
          "move-to-monitor-${n}" = [];
          "move-to-workspace-${n}" = [];
        }))
      // (key-bindings arrow-directions
        (n: v: {
          "switch-to-workspace-${n}" = ["<Control>${v}"];
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
      logout = []; # NOTE: this means shutdown in Gnome.
      magnifier = [];
      magnifier-zoom-in = ["<Super>equal"];
      magnifier-zoom-out = ["<Super>minus"];
      screenreader = [];
      screensaver = ["<Super>q"]; # NOTE: this means lock screen in Gnome
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
      // (key-bindings workspaces
        (n: _: {
          "switch-to-application-${n}" = []; # conflicts with switch-to-workspace-${n}
        }));

    "org/gnome/shell/extensions/pop-shell" =
      {
        tile-orientation = ["<Super>r"];
        tile-enter = ["<Super>w"];
        management-orientation = ["r"];
        # (un)float a window
        toggle-floating = ["<Super>f"];
        # (un)float all windows
        # FIXME: following key binding hangs gnome
        # toggle-tiling = ["<Shift><Super>f"];
      }
      // (key-bindings directions
        (d: v: {
          # Focus window {direction}
          "focus-${d}" = ["<Super>${v}"];
          # Move window to the workspace at {direction}
          "pop-workspace-${d}" = ["<Shift><Super>${v}"];
          # Move window to the monitor at {direction}
          "pop-monitor-${d}" = ["<Control><Super>${v}"];

          # when "tile-enter"ed

          # Move window {direction}
          "tile-move-${d}" = [v];
          # Resize window {direction}
          "tile-resize-${d}" = ["<Control>${v}"];
          # Swap window {direction}
          "tile-swap-${d}" = ["<Shift>${v}"];
        }));
  };
}
