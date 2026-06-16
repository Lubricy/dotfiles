{
  pkgs,
  lib,
  config,
  ...
}: let
  headless = config.dot.profiles.headlessServer.enable;
in {
  # auto upgrade nix to the unstable version
  # https://github.com/NixOS/nixpkgs/blob/nixos-unstable/pkgs/tools/package-management/nix/default.nix#L284
  nix.package = pkgs.nixVersions.latest;
  time.timeZone = lib.mkDefault "Asia/Harbin";
  environment.systemPackages = with pkgs;
    [
    git # used by nix flakes

    # archives
    xz
    zstd

    # Text Processing
    # Docs: https://github.com/learnbyexample/Command-line-text-processing
    gnugrep # GNU grep, provides `grep`/`egrep`/`fgrep`
    gnused # GNU sed, very powerful(mainly for replacing text in files)
    gawk # GNU awk, a pattern scanning and processing language
    jq # A lightweight and flexible command-line JSON processor

    # networking tools
    # iperf3
    # ldns # replacement of `dig`, it provide the command `drill`
    # wget
    curl
    # aria2 # A lightweight multi-protocol & multi-source command-line download utility
    # socat # replacement of openbsd-netcat
    # nmap # A utility for network discovery and security auditing

    # misc
    # findutils
    gnutar
    rsync
  ]
  ++ lib.optionals (!headless) [
    git-lfs # used by huggingface models
    zip
    # unzipNLS
    p7zip
    mtr # A network diagnostic tool
    dnsutils # `dig` + `nslookup`
    ipcalc # it is a calculator for the IPv4/v6 addresses
    file
    which
    tree

    # nix utils
    nvd
  ];

  programs.zsh.enable = true;
  programs.nix-index-database.comma.enable = lib.mkDefault (!headless);
}
