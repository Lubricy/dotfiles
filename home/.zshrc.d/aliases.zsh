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
  docker run --rm -i -t -v nix-store:/nix -v nix-home:/root -v $(pwd):/src --workdir /src -e http_proxy -e https_proxy -e HTTP_PROXY -e HTTPS_PROXY -e no_proxy -e NO_PROXY "$@" nixos/nix 
}
if command -v docker >/dev/null 2>&1; then
  alias nix-docker=foo_nix
fi

if command -v parallel >/dev/null 2>&1; then
  alias par='parallel --progress'
  alias parp='parallel --progress --pipe'
fi
foo_venv () {
  dir=${1:-.venv}
  if [ ! -d $dir ]; then
    echo "creating venv in $dir"
    python3 -m venv $dir
  fi
  source $dir/bin/activate
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
		if [ -e "environment.yml" ]; then
			# echo "environment.yml file found"
			ENV=$(head -n 1 environment.yml | cut -f2 -d ' ')
			# Check if you are already in the environment
			if [[ $PATH != *$ENV* ]]; then
				# Check if the environment exists
				conda activate $ENV
				if [ $? -eq 0 ]; then
					:
				else
					# Create the environment and activate
					echo "Conda env '$ENV' doesn't exist."
					conda env create -q
					conda activate $ENV
				fi
			fi
		fi
  elif [ "$1" = "sync" ]; then
		if [ -e "environment.yml" ]; then
			# echo "environment.yml file found"
			ENV=$(head -n 1 environment.yml | cut -f2 -d ' ')
			# Check if you are already in the environment
			if [[ $PATH != *$ENV* ]]; then
				# Check if the environment exists
				conda activate $ENV
				if [ $? -eq 0 ]; then
          :
				else
					# Create the environment and activate
					echo "Conda env '$ENV' doesn't exist."
					conda env create -q
					conda activate $ENV
				fi
			fi
      conda env export | sed '/prefix/d' > environment.yml
		fi
  else
    ENV="$(basename $(pwd))"
    conda create -n $ENV python="$1"
    conda activate $ENV
    conda env export | sed '/prefix/d' > environment.yml
  fi
}
if command -v conda >/dev/null 2>&1; then
  alias cvenv=foo_conda-env
fi
if command -v pipenv >/dev/null 2>&1; then
  alias venv=foo_pipenv
else
  alias venv=foo_venv
fi

if command -v rsync >/dev/null 2>&1; then
  alias lcp='rsync -ah --info=progress2 --stats'
  alias lmv='rsync -ah --remove-source-files --info=progress2 --stats'
fi

alias ec=emacsclient
