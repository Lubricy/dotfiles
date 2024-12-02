{
  lib,
  pkgs,
  nixpkgs-unstable,
  ...
}: {
  imports = [
    "${nixpkgs-unstable}/nixos/modules/services/misc/ollama.nix"
  ];
  disabledModules = [
    "services/misc/ollama.nix"
  ];
  services.ollama = {
    enable = lib.mkDefault false;
    package = pkgs.unstable.ollama;
    acceleration = "cuda";
  };
}
