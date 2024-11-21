{pkgs, ...}: {
  home.packages = [pkgs.randomize-wallpaper];
  # This timer runs every 5 minutes to invoke matugen with random image
  systemd.user.services.randomize-wallpaper = {
    Unit = {
      Description = "randomize wallpaper service";
    };
    Service = {
      Type = "oneshot";
      ExecStart = "${pkgs.randomize-wallpaper}/bin/randomize-wallpaper";
    };
    Environment = {};
    Install.WantedBy = ["default.target"];
  };

  systemd.user.timers.randomize-wallpaper = {
    Unit = {
      Description = "timer for randomize-wallpaper service";
      Requires = "randomize-wallpaper.service";
    };
    Timer = {
      Unit = "randomize-wallpaper.service";
      OnBootSec = "1m";
      OnUnitActiveSec = "30m";
    };
    Install.WantedBy = ["timers.target"];
  };
}
