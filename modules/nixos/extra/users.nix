{
  lib,
  config,
  pkgs,
  ...
}: let
  cfg = config.dot.defaultUser;
in {
  config = lib.mkIf cfg.enable {
    users.mutableUsers = lib.mkDefault true;
    users.users.${cfg.username} = {
      isNormalUser = true;
      extraGroups = ["wheel" "networkmanager" "dialout" "input"]; # Enable ‘sudo’ for the user.
      home = "/home/${cfg.username}";
      shell = pkgs.zsh;
      openssh.authorizedKeys.keyFiles = cfg.authorizedKeyFiles;
      openssh.authorizedKeys.keys = cfg.authorizedKeys;
    };
  };
}
