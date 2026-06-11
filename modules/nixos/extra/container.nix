{
  lib,
  config,
  pkgs,
  ...
}: let
  inherit (lib) mkOption mkEnableOption mkIf mkMerge;
  cfg = config.dot.features.container;
in {
  options.dot.features.container = {
    enable = mkEnableOption "enable podman";
    dockerComposePkg = mkOption {
      type = lib.types.package;
      default = pkgs.docker-compose;
      description = "the package used for docker compose";
    };
  };
  config = mkIf cfg.enable (mkMerge [
    {
      virtualisation.podman = {
        enable = true;
        autoPrune.enable = true;
        dockerCompat = true;
        defaultNetwork.settings.dns_enabled = true;
      };
      environment.systemPackages = [
        cfg.dockerComposePkg
      ];
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
