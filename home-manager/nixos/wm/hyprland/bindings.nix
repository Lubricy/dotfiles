{
  wayland.windowManager.hyprland.settings = {
    bind = [
      "$mod,P,exec,alacritty"
      "$mod,Q,exec,eww open powermenu"
      "$mod,W,exec,eww open overview"
      "$mod,N,exec,org-capture"
      "$mod,X,killactive"
      "$mod SHIFT,Q,exec,uwsm stop"
    ];
    bindm = [
      # mouse movements
      "$mod, mouse:272, movewindow"
      "$mod, mouse:273, resizewindow"
      "$mod SHIFT, mouse:272, resizewindow"
    ];
  };
}
