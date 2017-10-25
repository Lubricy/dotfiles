if [[ $(uname) =~ Darwin ]]; then
  alias ls="ls -G -F"
else
  alias ls="ls --color -F"
fi
foo_cd () {
  cd "$@";
  ls
}
alias cd=foo_cd
if command -v nvim >/dev/null 2>&1; then
  alias vim=nvim
  alias vi=nvim
fi
if command -v htop >/dev/null 2>&1; then
  alias top=htop
fi

foo_nix () {
  docker run --rm -it -v nix-store:/nix -v nix-home:/root -v $(pwd):/src --workdir /src -e http_proxy -e HTTPS_PROXY "$@" nixos/nix zsh
}
if command -v docker >/dev/null 2>&1; then
  alias nix=foo_nix
fi
