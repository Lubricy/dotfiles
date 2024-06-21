{pkgs, config, lib,...}:
with lib;
let
  cfg = config.modules.services.scriptkit;
in {
  options.modules.services.scriptkit = {
    enable = mkEnableOption "Script Kit Launcher";
  };
  config = mkIf cfg.enable (mkMerge [{
    home.packages = with pkgs; [
      scriptkit
    ];

    launchd.enable = true;
    launchd.agents.scriptkit = {
      enable = true;
      config = {
        ProgramArguments = [
          "${pkgs.bash}/bin/bash"
          "-l"
          "-c"
          "${pkgs.scriptkit}/Applications/Kit.app/Contents/MacOS/Kit"
        ];
        EnvironmentVariables = {
          KNODE = "${config.xdg.configHome}/scriptkit/knode";
          KENV = "${config.xdg.configHome}/scriptkit/kenv";
          KIT = "${config.xdg.configHome}/scriptkit/kit";
        };
        StandardErrorPath = "${config.home.homeDirectory}/Library/Logs/scriptkit-daemon.stderr.log";
        StandardOutPath = "${config.home.homeDirectory}/Library/Logs/scriptkit-daemon.stdout.log";
        RunAtLoad = true;
        KeepAlive = true;
      };
    };

  }]);
}
