# PROFILE_STARTUP=false
if [[ "$PROFILE_STARTUP" == true ]]; then
    # http://zsh.sourceforge.net/Doc/Release/Prompt-Expansion.html
    PS4=$'%D{%H:%M:%S.%.} %N:%i> '
    exec 3>&2 2>$HOME/tmp/startlog.$$
    setopt xtrace prompt_subst
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

if [ -d $HOME/.zshrc.d ]; then
	for file in $HOME/.zshrc.d/*.zsh; do
		source $file
	done
fi

[[ -f $HOME/.nix-profile/etc/profile.d/nix.sh ]] && . $HOME/.nix-profile/etc/profile.d/nix.sh
#test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

# tabtab source for serverless package
# uninstall by removing these lines or running `tabtab uninstall serverless`
[[ -f $HOME/.homebrew/lib/node_modules/serverless/node_modules/tabtab/.completions/serverless.zsh ]] && . $HOME/.homebrew/lib/node_modules/serverless/node_modules/tabtab/.completions/serverless.zsh
# tabtab source for sls package
# uninstall by removing these lines or running `tabtab uninstall sls`
[[ -f $HOME/.homebrew/lib/node_modules/serverless/node_modules/tabtab/.completions/sls.zsh ]] && . $HOME/.homebrew/lib/node_modules/serverless/node_modules/tabtab/.completions/sls.zsh
# tabtab source for slss package
# uninstall by removing these lines or running `tabtab uninstall slss`
[[ -f $HOME/.homebrew/lib/node_modules/serverless/node_modules/tabtab/.completions/slss.zsh ]] && . $HOME/.homebrew/lib/node_modules/serverless/node_modules/tabtab/.completions/slss.zsh
## >>> conda initialize >>>
## !! Contents within this block are managed by 'conda init' !!
#__conda_setup="$('$HOME/.homebrew/Caskroom/miniconda/base/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
#if [ $? -eq 0 ]; then
#    eval "$__conda_setup"
#else
#    if [ -f "$HOME/.homebrew/Caskroom/miniconda/base/etc/profile.d/conda.sh" ]; then
#        . "$HOME/.homebrew/Caskroom/miniconda/base/etc/profile.d/conda.sh"
#    else
#        export PATH="$HOME/.homebrew/Caskroom/miniconda/base/bin:$PATH"
#    fi
#fi
#unset __conda_setup
## <<< conda initialize <<<

# Entirety of my startup file... then
if [[ "$PROFILE_STARTUP" == true ]]; then
    unsetopt xtrace
    exec 2>&3 3>&-
fi
