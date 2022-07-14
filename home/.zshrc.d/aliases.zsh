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
alias vi=vim
if command -v nvim >/dev/null 2>&1; then
  alias vim=nvim
  alias vi=nvim
fi
if command -v htop >/dev/null 2>&1; then
  alias top=htop
fi
if command -v bat >/dev/null 2>&1; then
  alias cat=bat
fi

foo_nix () {
  docker run --rm -i -t -v nix-store:/nix -v nix-home:/root -v $(pwd):/src --workdir /src -e http_proxy -e https_proxy -e HTTP_PROXY -e HTTPS_PROXY -e no_proxy -e NO_PROXY "$@" nixos/nix 
}
if command -v docker >/dev/null 2>&1; then
  alias nix-docker=foo_nix
fi

if command -v parallel >/dev/null 2>&1; then
  alias par='parallel --progress'
  alias parp='parallel --progress --pipe'
fi

verlte() {
    [  "$1" = "`echo -e "$1\n$2" | sort -V | head -n1`" ]
}

verlt() {
    [ "$1" = "$2" ] && return 1 || verlte $1 $2
}


foo_venv () {
  local dir=${1:-.venv}
  local name=${1:-$(basename $(pwd))}
  if [ ! -d $dir ]; then
    local venv_options=("$dir" "--prompt" "$name")
    if verlte "3.9" "$(python3 -V | cut -d ' ' -f2)"; then
      venv_options+=("--upgrade-deps")
    else
      local manual_update_pip=1
    fi
    echo "creating $dir for $name ..."
    python3 -m venv "${venv_options[@]}"
  fi
  source $dir/bin/activate

  if [ "${manual_update_pip+x}" ]; then
    pip install --upgrade pip setuptools
  fi
}
foo_pipenv () {
  if [ -z "$1" ]; then
    pipenv shell
  else
    pipenv shell --python "$1"
  fi
}
foo_conda-env () {
	source $(conda info --base)/etc/profile.d/conda.sh
  if [ -z "$1" ]; then
    PYVER="3"
  else
    PYVER="$1"
  fi
  if [ -e "environment.yml" ]; then
    echo "environment.yml file found"
    ENV=$(head -n 1 environment.yml | cut -f2 -d ' ')
    CMD="conda env create"
  else
    ENV="./.venv"
    CMD="conda create python=$PYVER"
  fi
  # Check if you are already in the environment
  if [[ $PATH != *$ENV* ]]; then
    # Check if the environment exists
    conda activate $ENV
    if [ $? -eq 0 ]; then
      :
    else
      # Create the environment and activate
      echo "Conda env '$ENV' doesn't exist."
      if [[ $ENV != *.venv* ]]; then
        $SHELL -c "$CMD -q"
      else
        $SHELL -c "$CMD -q --prefix $ENV"
      fi
      conda activate $ENV
    fi
  fi
  conda env export | sed "s#^name:.*\$#name: $ENV#g;/prefix/d" > environment.yml
}
if command -v conda >/dev/null 2>&1; then
  alias cvenv=foo_conda-env
fi
if command -v pipenv >/dev/null 2>&1; then
  alias pvenv=foo_pipenv
fi
alias venv=foo_venv

if command -v rsync >/dev/null 2>&1; then
  alias lcp='rsync -ah --info=progress2 --stats'
  alias lmv='rsync -ah --remove-source-files --info=progress2 --stats'
fi

alias ec=emacsclient
alias urldecode="python3 -c 'import sys, urllib.parse as ul; \
	print(ul.unquote_plus(next(iter(sys.argv[1:2]), None) or sys.stdin.read()), end=\"\")'"

alias urlencode="python3 -c 'import sys, urllib.parse as ul; \
	print(ul.quote_plus(next(iter(sys.argv[1:2]), None) or sys.stdin.read()), end=\"\")'"


alias pwe="PASSWORD_STORE_ENABLE_EXTENSIONS=true pass fzf -e"
pwn() {
	PASSWORD_STORE_ENABLE_EXTENSIONS=true pass fzf -s
}
alias pwc="PASSWORD_STORE_ENABLE_EXTENSIONS=true pass fzf"
function rot() { 
	cat - <(echo -n $1) | openssl dgst -binary -sha256 | openssl base64 | head -c 8
}
