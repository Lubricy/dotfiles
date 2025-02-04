{
  lib,
  config,
  ...
}: {
  options.modules.deployrs.enable = lib.mkEnableOption "Enable deploy-rs";
  config = lib.mkIf config.modules.deployrs.enable {
    users.users.deployrs = {
      isSystemUser = true;
      useDefaultShell = true;
      group = "deployrs";
      openssh.authorizedKeys.keys = config.vars.authorizedKeys;
    };
    users.groups.deployrs = {};
    security.sudo.configFile = "deployrs ALL=(ALL) NOPASSWD:ALL";

    nix.settings.trusted-users = ["deployrs"];
  };
}
