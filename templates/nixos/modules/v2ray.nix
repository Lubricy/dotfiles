{
  lib,
  pkgs,
  ...
}: {
  networking.proxy = {
    default = "http://127.0.0.1:8000";
    noProxy = "127.0.0.1,::1,localhost,.local,.localdomain";
  };
  services.v2ray = {
    enable = true;
    package = pkgs.unstable.v2ray;
    config = lib.importJSON ./config.json;
  };
}
