# Lines configured by zsh-newuser-install
HISTFILE=~/.zsh_history
HISTSIZE=1000
SAVEHIST=1000
setopt inc_append_history
setopt hist_ignore_dups 
setopt hist_ignore_space
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '~/.zshrc'
fpath=($HOME/.homesick/repos/homeshick/completions $fpath)
autoload -Uz compinit
compinit
# End of lines added by compinstall

zstyle ':comletion:*' menu select

# backward delete path
my-backward-delete-word() {
	local WORDCHARS=${WORDCHARS/\//}
	zle backward-delete-word
}
zle -N my-backward-delete-word
bindkey '^W' my-backward-delete-word


export PATH=$HOME/.local/bin:$PATH
if [ -d $HOME/.zshrc.d ]; then
	for file in $HOME/.zshrc.d/*.zsh; do
		source $file
	done
fi

. /Users/a578203/.nix-profile/etc/profile.d/nix.sh
#test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

# tabtab source for serverless package
# uninstall by removing these lines or running `tabtab uninstall serverless`
[[ -f /Users/a578203/.homebrew/lib/node_modules/serverless/node_modules/tabtab/.completions/serverless.zsh ]] && . /Users/a578203/.homebrew/lib/node_modules/serverless/node_modules/tabtab/.completions/serverless.zsh
# tabtab source for sls package
# uninstall by removing these lines or running `tabtab uninstall sls`
[[ -f /Users/a578203/.homebrew/lib/node_modules/serverless/node_modules/tabtab/.completions/sls.zsh ]] && . /Users/a578203/.homebrew/lib/node_modules/serverless/node_modules/tabtab/.completions/sls.zsh
# tabtab source for slss package
# uninstall by removing these lines or running `tabtab uninstall slss`
[[ -f /Users/a578203/.homebrew/lib/node_modules/serverless/node_modules/tabtab/.completions/slss.zsh ]] && . /Users/a578203/.homebrew/lib/node_modules/serverless/node_modules/tabtab/.completions/slss.zsh
## >>> conda initialize >>>
## !! Contents within this block are managed by 'conda init' !!
#__conda_setup="$('/Users/a578203/.homebrew/Caskroom/miniconda/base/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
#if [ $? -eq 0 ]; then
#    eval "$__conda_setup"
#else
#    if [ -f "/Users/a578203/.homebrew/Caskroom/miniconda/base/etc/profile.d/conda.sh" ]; then
#        . "/Users/a578203/.homebrew/Caskroom/miniconda/base/etc/profile.d/conda.sh"
#    else
#        export PATH="/Users/a578203/.homebrew/Caskroom/miniconda/base/bin:$PATH"
#    fi
#fi
#unset __conda_setup
## <<< conda initialize <<<

