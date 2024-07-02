{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.skhd;
in

{
  options.services.skhd = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = "Whether to enable the skhd hotkey daemon.";
    };

    package = mkOption {
      type = types.package;
      default = pkgs.skhd;
      description = "This option specifies the skhd package to use.";
    };

    skhdConfig = mkOption {
      type = types.lines;
      default = "";
      example = "alt + shift - r   :   chunkc quit";
      description = "Config to use for {file}`skhdrc`.";
    };
  };

  config = mkIf cfg.enable {

    home.packages = [ cfg.package ];

    xdg.configFile."skhd/skhdrc".text = cfg.skhdConfig;

    launchd.agents.skhd = {
      enable = true;
      config = {
        ProgramArguments = [ "${cfg.package}/bin/skhd" ];
        KeepAlive = true;
        ProcessType = "Interactive";
        StandardErrorPath = "${config.home.homeDirectory}/Library/Logs/skhd.stderr.log";
        StandardOutPath = "${config.home.homeDirectory}/Library/Logs/skhd.stdout.log";
      };
    };
  };
}
