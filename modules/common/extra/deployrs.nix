{
  lib,
  config,
  ...
}: {
  options.dot.features.deployrs.enable = lib.mkEnableOption "Enable deploy-rs";
  config = lib.mkIf config.dot.features.deployrs.enable {
    users.users.deployrs = {
      isSystemUser = true;
      useDefaultShell = true;
      group = "deployrs";
      openssh.authorizedKeys.keyFiles = config.dot.defaultUser.authorizedKeyFiles;
      openssh.authorizedKeys.keys = config.dot.defaultUser.authorizedKeys;
    };
    users.groups.deployrs = {};
    security.sudo.configFile = "deployrs ALL=(ALL) NOPASSWD:ALL";

    nix.settings.trusted-users = ["deployrs"];
  };
}
