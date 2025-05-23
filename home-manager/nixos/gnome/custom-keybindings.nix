{lib, ...}: let
  inherit (lib) mapAttrs' mapAttrsToList;
  prefix = "org/gnome/settings-daemon/plugins/media-keys";
  dconf-settings =
    mapAttrs' (k: v: {
      name = "${prefix}/custom-keybindings/${k}";
      value = v;
    })
    bindings;
  custom-keybindings = mapAttrsToList (k: _: "/${k}/") dconf-settings;
  bindings = {
    terminal = {
      name = "Terminal";
      binding = "<Super>Return";
      command = "alacritty";
    };
    editor = {
      name = "Emacs Client";
      binding = "<Super>e";
      command = "emacsclient -c";
    };
    restartEditor = {
      name = "Restart Emacs";
      binding = "<Shift><Super>e";
      command = "systemctl restart --user emacs && emacsclient -c";
    };
    browser = {
      name = "Firefox";
      binding = "<Super>b";
      command = "firefox";
    };
    capture = {
      name = "Take Note (Org-Capture)";
      binding = "<Super>n";
      command = "org-capture";
    };
  };
in {
  dconf.settings =
    {
      "${prefix}" = {
        inherit custom-keybindings;
      };
    }
    // dconf-settings;
}
