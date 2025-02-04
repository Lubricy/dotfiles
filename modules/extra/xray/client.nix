{
  lib,
  pkgs,
  config,
  ...
}: let
  inherit (lib) mkIf mkOption mkEnableOption types mapAttrsToList;
in {
  options.modules.xrayClient = {
    enable = mkEnableOption "enable client xray & config";
    configPath = mkOption {
      type = types.str;
      default = "xray/config.json";
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
          address = mkOption {
            type = types.str;
          };
          port = mkOption {
            type = types.int;
            default = 443;
          };
          serverName = mkOption {
            type = types.str;
            default = config.modules.xrayClient.address;
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
    cfg = config.modules.xrayClient;
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
            serverName ? "",
            streamSettings ? {},
            ...
          }: {
            network = network;
            security = "tls";
            tlsSettings = {
              serverName = serverName; # //修改为使用本应用的域名。（address 使用 IP 后必须配置，否则可删除。）
              alpn = ["h2"]; # //若服务端（含使用 CDN 流量中转的服务器）支持 HTTP/3 传输其值可改为 h3
              fingerprint = "chrome"; # //模拟 TLS 指纹，可任一 chrome、firefox、safari、ios、edge、qq、random、randomized 选项配置。
            };
            "${network}Settings" = {path = path;} // streamSettings;
          };
          mkOutbound = name: attrs: {
            protocol = attrs.protocol;
            # sniffing = {
            #   destOverride = [
            #     "http"
            #     "tls"
            #     "quic"
            #   ];
            #   enabled = true;
            # };
            settings = {
              vnext = [
                {
                  address = attrs.address;
                  port = attrs.port;
                  users = [
                    {
                      id = attrs.secret;
                      encryption = "none";
                    }
                  ];
                }
              ];
              decryption = "none";
            };
            streamSettings = mkStreamSettings attrs;
          };
        in
          builtins.toJSON {
            inbounds = [
              {
                port = "1080";
                protocol = "socks";
                settings = {
                  udp = true;
                };
              }
              {
                port = "8000";
                protocol = "http";
                settings = {};
              }
            ];
            log = {
              loglevel = "info";
              # error = "/dev/stderr";
              # access = "/dev/stderr";
            };
            outbounds = mapAttrsToList mkOutbound cfg.profiles;
            routing = {
              # rules = [
              #   {
              #     type = "field";
              #     domain = ["geosite:cn"]; #中国大陆主流网站的域名
              #     outboundTag = "direct"; # 与下 outbounds 中 tag 对应
              #   }
              #   {
              #     type = "field";
              #     ip = [
              #       "geoip:cn" # client.nix IP
              #       "geoip:private" # 私有地址 IP，如路由器等。
              #     ];
              #     outboundTag = "direct"; #与下 outbounds 中 tag 对应
              #   }
              # ];
            };
          };
      };
    };
}
