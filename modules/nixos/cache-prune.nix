{
  lib,
  config,
  pkgs,
  ...
}: let
  inherit (lib) mkEnableOption mkIf mkMerge mkOption types;
  cfg = config.dot.optimize;

  packageInstalledByPrefixes = packages: prefixes:
    lib.any (pkg: let
      pname = pkg.pname or "";
      name = lib.getName pkg;
    in
      lib.any (prefix: lib.hasPrefix prefix pname || lib.hasPrefix prefix name) prefixes) packages;

  dockerInstalled =
    lib.attrByPath ["virtualisation" "docker" "enable"] false config
    || packageInstalledByPrefixes config.environment.systemPackages ["docker"];

  podmanInstalled =
    lib.attrByPath ["virtualisation" "podman" "enable"] false config
    || packageInstalledByPrefixes config.environment.systemPackages ["podman"];

  retentionFlags = job: ["--filter" "until=${job.retention}"] ++ job.extraArgs;
in {
  options.dot.optimize = {
    enable = mkEnableOption "automatic cache pruning";

    docker = {
      enable = mkOption {
        type = types.bool;
        default = dockerInstalled;
        defaultText = lib.literalExpression "true if Docker is installed, otherwise false";
        description = "Whether to run Docker cache pruning.";
      };
      package = mkOption {
        type = types.package;
        default = pkgs.docker;
        description = "Package providing the `docker` executable.";
      };
      retention = mkOption {
        type = types.str;
        default = "4320h";
        example = "2160h";
        description = "Age threshold passed to `docker system prune --filter until=...`.";
      };
      extraArgs = mkOption {
        type = types.listOf types.str;
        default = [];
        description = "Additional arguments passed to `docker system prune`.";
      };
      schedule = mkOption {
        type = types.str;
        default = "monthly";
        description = "Timer `OnCalendar` value for Docker pruning.";
      };
      randomizedDelaySec = mkOption {
        type = types.str;
        default = "1d";
        description = "Timer jitter for Docker pruning.";
      };
      persistent = mkOption {
        type = types.bool;
        default = true;
        description = "Whether missed Docker prune runs should execute on next boot.";
      };
    };

    podman = {
      enable = mkOption {
        type = types.bool;
        default = podmanInstalled;
        defaultText = lib.literalExpression "true if Podman is installed, otherwise false";
        description = "Whether to run Podman cache pruning.";
      };
      package = mkOption {
        type = types.package;
        default = pkgs.podman;
        description = "Package providing the `podman` executable.";
      };
      retention = mkOption {
        type = types.str;
        default = "4320h";
        example = "2160h";
        description = "Age threshold passed to `podman system prune --filter until=...`.";
      };
      extraArgs = mkOption {
        type = types.listOf types.str;
        default = [];
        description = "Additional arguments passed to `podman system prune`.";
      };
      schedule = mkOption {
        type = types.str;
        default = "monthly";
        description = "Timer `OnCalendar` value for Podman pruning.";
      };
    };
  };

  config = mkIf cfg.enable (mkMerge [
    (mkIf cfg.docker.enable {
      virtualisation.docker.autoPrune = {
        enable = true;
        dates = cfg.docker.schedule;
        randomizedDelaySec = cfg.docker.randomizedDelaySec;
        persistent = cfg.docker.persistent;
        flags = retentionFlags cfg.docker;
      };
    })
    (mkIf cfg.podman.enable {
      virtualisation.podman.autoPrune = {
        enable = true;
        dates = cfg.podman.schedule;
        flags = retentionFlags cfg.podman;
      };
    })
  ]);
}
