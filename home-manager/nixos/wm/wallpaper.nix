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
