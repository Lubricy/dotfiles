{
  lib,
  config,
  ...
}: let
  inherit (lib) types mkOption mkEnableOption mkIf mkMerge;
  cfg = config.features.visualization;
in {
  options.features.visualization = {
    enable = mkEnableOption "enable visualization";
    engine = mkOption {
      type = types.enum ["docker" "podman"];
      default = "podman";
    };
    emulatedSystems = config.options.boot.binfmt.emulatedSystems;
  };
  config = mkIf cfg.enable (mkMerge [
    {
      virtualisation.podman = {
        enable = true;
        autoPrune.enable = true;
        dockerCompat = true;
        defaultNetwork.settings.dns_enabled = true;
      };
    }
    (mkIf config.dot.features.nvidia.enable {
      hardware.nvidia-container-toolkit.enable = true;
    })
    (mkIf ((config ? wsl) && config.wsl.enable) {
      hardware.nvidia-container-toolkit.mount-nvidia-executables = false;
      systemd.services.nvidia-container-toolkit-cdi-generator = {
        serviceConfig.Environment = ''LD_LIBRARY_PATH="/usr/lib/wsl/lib"'';
      };
    })
  ]);
}
