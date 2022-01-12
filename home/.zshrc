# PROFILE_STARTUP=false
if [[ "$PROFILE_STARTUP" == true ]]; then
    zmodload zsh/zprof
fi
# Lines configured by zsh-newuser-install
HISTFILE=~/.zsh_history
HISTSIZE=1000
SAVEHIST=1000
setopt inc_append_history
setopt hist_ignore_dups 
setopt hist_ignore_space
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
# zstyle :compinstall filename '~/.zshrc'
# fpath=($HOME/.homesick/repos/homeshick/completions $fpath)

# backward delete path
my-backward-delete-word() {
	local WORDCHARS=${WORDCHARS/\//}
	zle backward-delete-word
}
zle -N my-backward-delete-word
bindkey '^W' my-backward-delete-word

if [ -d $HOME/.zshrc.d ]; then
	for file in $HOME/.zshrc.d/*.zsh; do
		source $file
	done
fi

typeset -U PATH
# Entirety of my startup file... then
if [[ "$PROFILE_STARTUP" == true ]]; then
    zprof
fi
[ -f "/Users/a578203/.ghcup/env" ] && source "/Users/a578203/.ghcup/env" # ghcup-env
