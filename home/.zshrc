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

#test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"
