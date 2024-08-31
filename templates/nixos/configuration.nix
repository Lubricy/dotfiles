let
  envVars = {
    # ZOOM_BACKGROUND_UUID = "545CCF0D-6026-475D-A46E-2A17188DE310";
  };
in {
  imports = [
    ./hardware-configuration.nix
  ];
  nix.envVars = envVars;
}
