{config, ...}: {
  ###################################################################################
  #
  #  Core configuration for nix-darwin
  #
  #  All the configuration options are documented here:
  #    https://daiderd.com/nix-darwin/manual/index.html#sec-options
  #
  # History Issues:
  #  1. Fixed by replace the determined nix-installer by the official one:
  #     https://github.com/LnL7/nix-darwin/issues/149#issuecomment-1741720259
  #
  ###################################################################################

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # Auto upgrade the nix-daemon service.
  services.nix-daemon.enable = true;

  # taken from github:nixos/nixpkgs/nixos/modules/config/shells-environment.nix
  environment.etc."set-environment".source = config.system.build.setEnvironment;
}
