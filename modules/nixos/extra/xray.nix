{
  lib,
  config,
  pkgs,
  ...
}: let
  cfg = config.dot.features.xray;
in {
  options.dot.features.xray = {
    enable = lib.mkEnableOption "XRay Client/Server";
    systemProxy = lib.mkEnableOption "set system proxy";
    configPath = lib.mkOption {
      type = lib.types.string;
      default = "shared/xray/config.json";
      description = "secret path in nix-secrets";
    };
  };
  config = lib.mkIf cfg.enable (
    lib.mkMerge [
      {
        dot.secrets = [cfg.configPath];
        services.xray = {
          enable = true;
          settingsFile = pkgs.emptyDirectory;
          package = pkgs.unstable.xray;
        };
        systemd.services.xray.serviceConfig = {
          LoadCredential = "config.json:${config.sops.secrets."${cfg.configPath}".path}";
          ExecStart = lib.mkForce "${config.services.xray.package}/bin/xray -config %d/config.json";
        };
      }
      (lib.mkIf cfg.systemProxy {
        networking.proxy = {
          default = "http://localhost:8000/";
          noProxy = "::1,127.0.0.1,localhost,10.0.0.0/8,172.0.0.0/8";
        };
      })
    ]
  );
}
