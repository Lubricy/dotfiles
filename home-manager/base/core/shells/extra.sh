nr() {
    PKG="$1"
    shift
    nix run "nixpkgs#$PKG" -- "$@"
}

init() {
    TPL="${1:-poetry}"
    nix init -t "dotfiles#$TPL"
}
