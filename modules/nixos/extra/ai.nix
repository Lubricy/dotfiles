{
  lib,
  config,
  ...
}: {
  options.dot.features.localAI.enable = lib.mkEnableOption "Local AI";

  config =
    lib.mkIf config.dot.features.localAI.enable
    (lib.mkMerge [
      {
        services.ollama.enable = true;
      }
      (lib.mkIf config.dot.features.nvidia.enable {
        services.ollama.acceleration = "cuda";
      })
    ]);
}
