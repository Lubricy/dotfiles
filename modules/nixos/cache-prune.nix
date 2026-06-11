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

  mkContainerScript = name: job:
    pkgs.writeShellApplication {
      name = "cache-prune-${name}";
      runtimeInputs = [job.package];
      text = ''
        set -euo pipefail

        if ! ${lib.getExe job.package} info >/dev/null 2>&1; then
          echo "${name}: runtime unavailable, skipping"
          exit 0
        fi

        exec ${lib.getExe job.package} system prune \
          --force \
          --filter "until=${job.retention}" \
          ${lib.escapeShellArgs job.extraArgs}
      '';
    };

  mkService = name: job: let
    script = mkContainerScript name job;
  in {
    description = "Prune ${name} cache and unused container artifacts";
    after = job.after;
    wants = job.wants;
    serviceConfig = {
      Type = "oneshot";
      ExecStart = lib.getExe script;
      Nice = 19;
      IOSchedulingClass = "idle";
      CPUSchedulingPolicy = "idle";
    };
  };

  mkTimer = job: {
    wantedBy = ["timers.target"];
    timerConfig = {
      OnCalendar = job.schedule;
      RandomizedDelaySec = job.randomizedDelaySec;
      Persistent = job.persistent;
    };
  };
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
      after = mkOption {
        type = types.listOf types.str;
        default = ["docker.service"];
        description = "Additional `After=` dependencies for the Docker prune service.";
      };
      wants = mkOption {
        type = types.listOf types.str;
        default = ["docker.service"];
        description = "Additional `Wants=` dependencies for the Docker prune service.";
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
      randomizedDelaySec = mkOption {
        type = types.str;
        default = "1d";
        description = "Timer jitter for Podman pruning.";
      };
      persistent = mkOption {
        type = types.bool;
        default = true;
        description = "Whether missed Podman prune runs should execute on next boot.";
      };
      after = mkOption {
        type = types.listOf types.str;
        default = [];
        description = "Additional `After=` dependencies for the Podman prune service.";
      };
      wants = mkOption {
        type = types.listOf types.str;
        default = [];
        description = "Additional `Wants=` dependencies for the Podman prune service.";
      };
    };
  };

  config = mkIf cfg.enable (mkMerge [
    (mkIf cfg.docker.enable {
      systemd.services.cache-prune-docker = mkService "docker" cfg.docker;
      systemd.timers.cache-prune-docker = mkTimer cfg.docker;
    })
    (mkIf cfg.podman.enable {
      systemd.services.cache-prune-podman = mkService "podman" cfg.podman;
      systemd.timers.cache-prune-podman = mkTimer cfg.podman;
    })
  ]);
}
