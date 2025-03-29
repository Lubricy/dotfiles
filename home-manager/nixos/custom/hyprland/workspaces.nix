{lib, ...}: let
  inherit (lib) concatMap range;
in {
  wayland.windowManager.hyprland.settings = {
    workspace = [
      "1, persistent:true, defaultName:Terminal"
      "2, persistent:true, defaultName:Browser"
      "3, persistent:true, defaultName:Editor"
    ];
    bind = concatMap (i: [
      "$mod,${toString i},workspace,${toString i}"
      "$mod SHIFT,${toString i},movetoworkspace,${toString i}"
    ]) (range 1 9);
  };
}
