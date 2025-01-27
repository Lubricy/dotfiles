{
  # https://nixos.wiki/wiki/Storage_optimization
  nix.optimise.automatic = true;

  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 90d";
    randomizedDelaySec = "10m";
  };

  programs.nix-ld.enable = true;
}
