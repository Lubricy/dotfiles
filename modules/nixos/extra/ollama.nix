{lib, ...}: {
  services.ollama = {
    enable = lib.mkDefault false;
    acceleration = "cuda";
  };
}
