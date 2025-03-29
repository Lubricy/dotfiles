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
    browser = {
      name = "Firefox";
      binding = "<Super>b";
      command = "firefox";
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
