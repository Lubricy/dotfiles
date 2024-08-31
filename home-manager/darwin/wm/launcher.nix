{
  pkgs,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.modules.launcher.fzf;
in {
  options.modules.launcher.fzf = {
    enable = mkEnableOption "FZF Launcher";
  };
  config = mkIf cfg.enable (mkMerge [
    {
      home.packages = with pkgs; [
        alacritty
        fzf
      ];
      services.skhd = {
        skhdConfig = lib.mkAfter ''
          alt - p : launcher
        '';
      };
      services.yabai = {
        extraConfig = lib.mkAfter ''
          yabai -m rule --add title='^Launcher$' manage=off grid=5:5:1:1:3:3
        '';
      };
    }
  ]);
}
