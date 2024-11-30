{pkgs, ...}: {
  home.packages = with pkgs; [
    randomize-wallpaper
    download-image
  ];
  # This timer runs every 5 minutes to invoke matugen with random image
  systemd.user.services.randomize-wallpaper = {
    Unit.Description = "randomize wallpaper service";
    Service = {
      Type = "oneshot";
      ExecStart = "${pkgs.randomize-wallpaper}/bin/randomize-wallpaper -n";
    };
    # Install.WantedBy = ["hyprland-session.target"];
  };
  systemd.user.services.swww = {
    Unit.Description = "swww wallpaper service";
    Service = {
      Type = "exec";
      Restart = "always";
      ExecStart = "${pkgs.swww}/bin/swww-daemon";
    };
    Install.WantedBy = ["hyprland-session.target"];
  };
  systemd.user.services.eww = {
    Unit = {
      Description = "eww widget service";
    };
    Service = {
      Type = "exec";
      Restart = "always";
      ExecStart = "${pkgs.eww}/bin/eww daemon --no-daemonize";
    };
    # Install.WantedBy = ["hyprland-session.target"];
  };

  systemd.user.timers.randomize-wallpaper = {
    Unit = {
      Description = "timer for randomize-wallpaper service";
      Requires = "randomize-wallpaper.service";
    };
    Timer = {
      Unit = "randomize-wallpaper.service";
      OnBootSec = "10m";
      OnUnitActiveSec = "30m";
    };
    Install.WantedBy = ["timers.target"];
  };
}
