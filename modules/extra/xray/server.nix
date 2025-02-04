{
  lib,
  pkgs,
  config,
  ...
}: let
  inherit (lib) mkIf mkOption mkEnableOption types mapAttrsToList;
in {
  options.modules.xray = {
    enable = mkEnableOption "enable server xray & config";
    configPath = mkOption {
      type = types.str;
      default = "xray/config.json";
    };
    socksBaseDir = mkOption {
      type = types.str;
      default = "/dev/shm";
    };
    socksPermission = mkOption {
      type = types.str;
      default = "0666";
    };
    profiles = mkOption {
      type = types.attrsOf (types.submodule {
        options = {
          protocol = mkOption {
            type = types.enum ["vless" "vmess"];
          };
          network = mkOption {
            type = types.enum ["xhttp" "httpupgrade"];
          };
          path = mkOption {
            type = types.str;
          };
          secret = mkOption {
            type = types.str;
          };
        };
      });
    };
  };
  config = let
    cfg = config.modules.xray;
  in
    mkIf cfg.enable {
      services.xray = {
        enable = cfg.enable;
        package = pkgs.xray.overrideAttrs (old: rec {
          version = "24.12.15"; # usually harmless to omit
          src = pkgs.fetchFromGitHub {
            owner = "XTLS";
            repo = "Xray-core";
            rev = "v${version}";
            sha256 = "sha256-ZFfhiShl9v9CwqUc28vo4MgsqXUFSEqnlU2Kai07IGA=";
          };
          vendorHash = "sha256-PiKC8UYYmYEHfy1/sNURcuS0iZnba4kHipDiG9EOgr0=";
        });
        settingsFile = pkgs.emptyDirectory; # overriten in systemd services' ExecStart
      };

      systemd.services.xray.serviceConfig.LoadCredential = "config.json:${config.sops.templates.${cfg.configPath}.path}";
      systemd.services.xray.serviceConfig.ExecStart = lib.mkForce "${config.services.xray.package}/bin/xray -config %d/config.json";

      sops.templates.${cfg.configPath} = {
        restartUnits = [config.systemd.services.xray.name];
        content = let
          mkStreamSettings = {
            network,
            path,
            streamSettings ? {},
            ...
          }: {
            network = network;
            "${network}Settings" = {path = path;} // streamSettings;
          };
          mkInbound = name: attrs: {
            sniffing = {
              destOverride = [
                "http"
                "tls"
                "quic"
              ];
              enabled = true;
            };
            listen = "${cfg.socksBaseDir}/xray-uds-${name}.sock,${cfg.socksPermission}";
            protocol = attrs.protocol;
            settings = {
              clients = [
                {
                  # email = "2023@gmail.com";
                  id = attrs.secret;
                }
              ];
              decryption = "none";
            };
            streamSettings = mkStreamSettings attrs;
          };
        in
          builtins.toJSON {
            inbounds = mapAttrsToList mkInbound cfg.profiles;
            log = {
              loglevel = "debug";
              # error = "/dev/stderr";
              # access = "/dev/stderr";
            };
            outbounds = [
              {
                protocol = "freedom";
                settings = {};
              }
              {
                protocol = "blackhole";
                settings = {};
                tag = "block";
              }
            ];
            routing = {
              rules = [
                {
                  outboundTag = "block";
                  protocol = ["bittorrent"];
                  type = "field";
                }
              ];
            };
          };
      };
    };
}
