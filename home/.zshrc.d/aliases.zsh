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
  docker run --rm -it -v nix-store:/nix -v nix-home:/root -v $(pwd):/src --workdir /src -e http_proxy=$docker_proxy -e HTTPS_PROXY=$docker_proxy -e NO_PROXY -e no_proxy "$@" nixos/nix 
}
if command -v docker >/dev/null 2>&1; then
  alias nix=foo_nix
fi

foo_venv () {
  dir=${1:-.venv}
  if [ ! -d $dir ]; then
    echo "creating venv in $dir"
    python3 -m venv $dir
  fi
  source $dir/bin/activate
}
foo_pipvenv () {
  if [ -z "$1" ]; then
    pipenv shell
  else
    pipenv shell --python "$1"
  fi
}
if command -v pipenv >/dev/null 2>&1; then
  alias venv=pipenv
else
  alias venv=foo_venv
fi
alias ec=emacsclient
