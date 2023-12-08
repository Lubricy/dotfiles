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

if [ -d $HOME/.zshrc.d ]; then
	for file in $HOME/.zshrc.d/*.zsh; do
		source $file
	done
fi

my-backward-delete-word() {
	local WORDCHARS=${WORDCHARS/\//}
	zle backward-delete-word
}
zle -N my-backward-delete-word
bindkey -M emacs '^W' my-backward-delete-word
bindkey -M viins '^W' my-backward-delete-word
bindkey -M vicmd '^W' my-backward-delete-word

bindkey -r '^[[A'
bindkey -r '^[[B'
function __bind_history_keys() {
  bindkey -M emacs '^[[A' history-substring-search-up
  bindkey -M viins '^[[A' history-substring-search-up
  bindkey -M vicmd '^[[A' history-substring-search-up
  bindkey -M emacs '^[[B' history-substring-search-down
  bindkey -M viins '^[[B' history-substring-search-down
  bindkey -M vicmd '^[[B' history-substring-search-down
}

typeset -U PATH
# Entirety of my startup file... then
if [[ "$PROFILE_STARTUP" == true ]]; then
    zprof
fi
