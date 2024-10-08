{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.services.yabai;

  toYabaiConfig = opts:
    concatStringsSep "\n" (mapAttrsToList
      (p: v: "yabai -m config ${p} ${toString v}")
      opts);

  configFile =
    mkIf (cfg.config != {} || cfg.extraConfig != "")
    "${pkgs.writeScript "yabairc" (
      (
        if (cfg.config != {})
        then "${toYabaiConfig cfg.config}"
        else ""
      )
      + optionalString (cfg.extraConfig != "") ("\n" + cfg.extraConfig + "\n")
    )}";
in {
  options = with types; {
    services.yabai.enable = mkOption {
      type = bool;
      default = false;
      description = "Whether to enable the yabai window manager.";
    };

    services.yabai.package = mkOption {
      type = path;
      default = pkgs.yabai;
      description = "The yabai package to use.";
    };

    services.yabai.config = mkOption {
      type = attrs;
      default = {};
      example = literalExpression ''
        {
          focus_follows_mouse = "autoraise";
          mouse_follows_focus = "off";
          window_placement    = "second_child";
          window_opacity      = "off";
          top_padding         = 36;
          bottom_padding      = 10;
          left_padding        = 10;
          right_padding       = 10;
          window_gap          = 10;
        }
      '';
      description = ''
        Key/Value pairs to pass to yabai's 'config' domain, via the configuration file.
      '';
    };

    services.yabai.extraConfig = mkOption {
      type = lines;
      default = "";
      example = literalExpression ''
        yabai -m rule --add app='System Preferences' manage=off
      '';
      description = "Extra arbitrary configuration to append to the configuration file";
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable) {
      home.packages = [cfg.package];

      launchd.agents.yabai = {
        enable = true;
        config = {
          ProgramArguments =
            ["${cfg.package}/bin/yabai"]
            ++ optionals (cfg.config != {} || cfg.extraConfig != "") ["-c" configFile];

          KeepAlive = true;
          RunAtLoad = true;
          EnvironmentVariables = {
            PATH = "${cfg.package}/bin:/bin"; # yabai need `sh` exists in $PATH
          };

          StandardErrorPath = "${config.home.homeDirectory}/Library/Logs/yabai.stderr.log";
          StandardOutPath = "${config.home.homeDirectory}/Library/Logs/yabai.stdout.log";
        };
      };
    })
  ];
}
