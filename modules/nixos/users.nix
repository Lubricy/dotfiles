# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
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
      extraGroups = ["wheel" "networkmanager"]; # Enable ‘sudo’ for the user.
      home = "/home/${cfg.username}";
      shell = pkgs.zsh;
    };
  };
}
