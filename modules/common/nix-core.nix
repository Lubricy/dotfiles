{
  lib,
  nixpkgs,
  config,
  ...
}: {
  ###################################################################################
  #
  #  Core configuration
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
  nixpkgs.config.allowUnfree = lib.mkDefault true;

  # Disable auto-optimise-store because of this issue:
  #   https://github.com/NixOS/nix/issues/7273
  # "error: cannot link '/nix/store/.tmp-link-xxxxx-xxxxx' to '/nix/store/.links/xxxx': File exists"
  nix.settings.auto-optimise-store = lib.mkDefault false;

  nix.gc.automatic = lib.mkDefault false;

  # make `nix run nixpkgs#nixpkgs` use the same nixpkgs as the one used by this flake.
  nix.registry.nixpkgs.flake = nixpkgs;

  # make `nix repl '<nixpkgs>'` use the same nixpkgs as the one used by this flake.
  # discard all the default paths, and only use the one from this flake.
  environment.etc."nix/inputs/nixpkgs".source = "${nixpkgs}";
  nix.nixPath = lib.mkForce ["/etc/nix/inputs"];

  nix.settings = {
    # enable flakes globally
    experimental-features = ["nix-command" "flakes"];

    # given the users in this list the right to specify additional substituters via:
    #    1. `nixConfig.substituers` in `flake.nix`
    #    2. command line args `--options substituers http://xxx`
    trusted-users = lib.mkIf config.dot.defaultUser.enable [config.dot.defaultUser.username];

    # substituers that will be considered before the official ones(https://cache.nixos.org)
    substituters = [
      # cache mirror located in China
      # status: https://mirror.sjtu.edu.cn/
      # "https://mirror.sjtu.edu.cn/nix-channels/store"
      # status: https://mirrors.ustc.edu.cn/status/
      # "https://mirrors.ustc.edu.cn/nix-channels/store"

      # "https://nix-community.cachix.org"
      # cuda-maintainer's cache server
      # "https://cuda-maintainers.cachix.org"
    ];

    trusted-public-keys = [
      # "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      # "cuda-maintainers.cachix.org-1:0dq3bujKpuEPMCX6U4WylrUDZ9JyUG0VpVZa7CNfq5E="
    ];
    builders-use-substitutes = true;

    # prevent build inputs from being GC’d
    keep-outputs = true;
    # automatically run garbage collection whenever there is not enough space left
    # free up to 1GiB whenever there is less than 100MiB left
    min-free = 100 * 1024 * 1024;
    max-free = 1024 * 1024 * 1024;
  };
}
