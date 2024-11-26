{
  lib,
  config,
  ...
}: let
  inherit (lib) types mkOption mkEnableOption mkIf mkMerge;
  cfg = config.modules.visualization;
in {
  options.modules.visualization = {
    enable = mkEnableOption "enable visualization";
    engine = mkOption {
      type = types.enum ["docker" "podman"];
      default = "podman";
    };
    emulatedSystems = config.options.boot.binfmt.emulatedSystems;
  };
  config =
    mkIf cfg.enable
    (mkMerge [
      {
        boot.binfmt.emulatedSystems = cfg.emulatedSystems;
        hardware.nvidia-container-toolkit.enable = true;
        virtualisation.${cfg.engine} = {
          enable = true;
          autoPrune.enable = true;
        };
      }
      (mkIf (cfg.engine == "podman") {
        virtualisation.podman.dockerCompat = true;
      })
      {
      }
    ]);
}
