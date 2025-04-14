{
  lib,
  pkgs,
  config,
  nixpkgs-unstable,
  ...
}: {
  imports = [
    "${nixpkgs-unstable}/nixos/modules/services/misc/ollama.nix"
  ];
  disabledModules = [
    "services/misc/ollama.nix"
  ];
  options.dot.features.localAI.enable = lib.mkEnableOption "Local AI";

  config =
    lib.mkIf config.dot.features.localAI.enable
    (lib.mkMerge [
      {
        services.ollama = {
          enable = true;
          package = pkgs.unstable.ollama;
        };
      }
      (lib.mkIf config.dot.features.nvidia.enable {
        services.ollama.acceleration = "cuda";
      })
    ]);
}
