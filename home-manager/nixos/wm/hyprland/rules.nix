{lib, ...}: let
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
      window.title = "^(?!org-capture$).*$";
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
in {
  wayland.windowManager.hyprland.settings = {
    windowrulev2 = flatten (mapAttrsToList lines rules);
  };
}
