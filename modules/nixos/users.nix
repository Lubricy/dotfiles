# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  lib,
  config,
  pkgs,
  ...
}: {
  users.mutableUsers = lib.mkDefault true;
  users.users.${config.vars.username} = {
    isNormalUser = true;
    extraGroups = ["wheel" "networkmanager"]; # Enable ‘sudo’ for the user.
    home = "/home/${config.vars.username}";
    shell = pkgs.zsh;
  };
}
