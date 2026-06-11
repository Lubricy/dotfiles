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

  uvInstalled = packageInstalledByPrefixes config.home.packages ["uv"];
  npmInstalled = packageInstalledByPrefixes config.home.packages ["nodejs" "npm"];
  podmanInstalled = packageInstalledByPrefixes config.home.packages ["podman"];

  mkUserService = name: script: {
    Unit.Description = "Prune ${name} cache";
    Service = {
      Type = "oneshot";
      ExecStart = lib.getExe script;
      Nice = 19;
      IOSchedulingClass = "idle";
      CPUSchedulingPolicy = "idle";
    };
  };

  mkUserTimer = job: {
    Unit.Description = "Schedule ${job.name} cache pruning";
    Timer = {
      OnCalendar = job.schedule;
      RandomizedDelaySec = job.randomizedDelaySec;
      Persistent = job.persistent;
    };
    Install.WantedBy = ["timers.target"];
  };

  uvScript = job:
    pkgs.writeShellApplication {
      name = "user-cache-prune-uv";
      runtimeInputs = [job.package];
      text = ''
        set -euo pipefail

        export UV_LOCK_TIMEOUT=30

        exec ${lib.getExe job.package} cache prune ${lib.escapeShellArgs job.extraArgs}
      '';
    };

  npmScript = job:
    pkgs.writeShellApplication {
      name = "user-cache-prune-npm";
      runtimeInputs = [job.package];
      text = ''
        set -euo pipefail

        cache_dir="$(${lib.getExe job.package} config get cache 2>/dev/null || true)"
        if [ -z "$cache_dir" ] || [ ! -d "$cache_dir" ]; then
          echo "npm: cache directory missing, skipping"
          exit 0
        fi

        exec ${lib.getExe job.package} cache verify ${lib.escapeShellArgs job.extraArgs}
      '';
    };

  podmanScript = job:
    pkgs.writeShellApplication {
      name = "user-cache-prune-podman";
      runtimeInputs = [job.package];
      text = ''
        set -euo pipefail

        if ! ${lib.getExe job.package} info >/dev/null 2>&1; then
          echo "podman: runtime unavailable, skipping"
          exit 0
        fi

        exec ${lib.getExe job.package} system prune \
          --force \
          --filter "until=${job.retention}" \
          ${lib.escapeShellArgs job.extraArgs}
      '';
    };
in {
  options.dot.optimize = {
    enable = mkEnableOption "automatic cache pruning";

    uv = {
      enable = mkOption {
        type = types.bool;
        default = uvInstalled;
        defaultText = lib.literalExpression "true if uv is installed, otherwise false";
        description = "Whether to run `uv cache prune`.";
      };
      package = mkOption {
        type = types.package;
        default = pkgs.uv;
        description = "Package providing the `uv` executable.";
      };
      extraArgs = mkOption {
        type = types.listOf types.str;
        default = [];
        description = "Additional arguments passed to `uv cache prune`.";
      };
      schedule = mkOption {
        type = types.str;
        default = "monthly";
        description = "Timer `OnCalendar` value for uv cache pruning.";
      };
      randomizedDelaySec = mkOption {
        type = types.str;
        default = "1d";
        description = "Timer jitter for uv cache pruning.";
      };
      persistent = mkOption {
        type = types.bool;
        default = true;
        description = "Whether missed uv prune runs should execute on next login.";
      };
    };

    npm = {
      enable = mkOption {
        type = types.bool;
        default = npmInstalled;
        defaultText = lib.literalExpression "true if npm is installed, otherwise false";
        description = "Whether to run `npm cache verify`.";
      };
      package = mkOption {
        type = types.package;
        default = pkgs.nodejs;
        description = "Package providing the `npm` executable.";
      };
      extraArgs = mkOption {
        type = types.listOf types.str;
        default = [];
        description = "Additional arguments passed to `npm cache verify`.";
      };
      schedule = mkOption {
        type = types.str;
        default = "monthly";
        description = "Timer `OnCalendar` value for npm cache cleanup.";
      };
      randomizedDelaySec = mkOption {
        type = types.str;
        default = "1d";
        description = "Timer jitter for npm cache cleanup.";
      };
      persistent = mkOption {
        type = types.bool;
        default = true;
        description = "Whether missed npm cleanup runs should execute on next login.";
      };
    };

    podman = {
      enable = mkOption {
        type = types.bool;
        default = podmanInstalled;
        defaultText = lib.literalExpression "true if podman is installed, otherwise false";
        description = "Whether to run `podman system prune` as a user service.";
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
        description = "Timer `OnCalendar` value for Podman cache pruning.";
      };
      randomizedDelaySec = mkOption {
        type = types.str;
        default = "1d";
        description = "Timer jitter for Podman cache pruning.";
      };
      persistent = mkOption {
        type = types.bool;
        default = true;
        description = "Whether missed Podman prune runs should execute on next login.";
      };
    };
  };

  config = mkIf cfg.enable (mkMerge [
    (mkIf cfg.uv.enable {
      systemd.user.services.cache-prune-uv = mkUserService "uv" (uvScript cfg.uv);
      systemd.user.timers.cache-prune-uv = mkUserTimer (cfg.uv // {name = "uv";});
    })
    (mkIf cfg.npm.enable {
      systemd.user.services.cache-prune-npm = mkUserService "npm" (npmScript cfg.npm);
      systemd.user.timers.cache-prune-npm = mkUserTimer (cfg.npm // {name = "npm";});
    })
    (mkIf cfg.podman.enable {
      systemd.user.services.cache-prune-podman = mkUserService "podman" (podmanScript cfg.podman);
      systemd.user.timers.cache-prune-podman = mkUserTimer (cfg.podman // {name = "podman";});
    })
  ]);
}
