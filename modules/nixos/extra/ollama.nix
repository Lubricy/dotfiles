{
  lib,
  pkgs,
  ...
}: {
  imports = [
    "${pkgs.unstable}/nixos/modules/services/misc/ollama.nix"
  ];
  disabledModules = [
    "services/misc/ollama.nix"
  ];
  services.ollama = {
    enable = lib.mkDefault false;
    acceleration = "cuda";
  };
}
