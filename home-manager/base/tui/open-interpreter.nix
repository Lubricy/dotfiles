{
  pkgs,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.modules.tools.open-interpreter;
in {
  options.modules.tools.open-interpreter = {
    enable = mkEnableOption "Open Interpreter";
  };

  config = mkIf cfg.enable (mkMerge [
    {
      home.packages = with pkgs.unstable; [
        open-interpreter
      ];
    }
  ]);
}
