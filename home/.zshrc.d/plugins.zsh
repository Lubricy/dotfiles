if [[ ! -d ~/.zplug ]]; then
	git clone https://github.com/zplug/zplug ~/.zplug
	source ~/.zplug/init.zsh && zplug update
fi

source ~/.zplug/init.zsh

zplug "~/.zshrc.d/", as:theme, from:local, use:my.zshtheme
zplug "zsh-users/zsh-syntax-highlighting", defer:2
zplug "clvv/fasd", as:command, use:fasd
zplug "plugins/docker", from:oh-my-zsh, if:"(( $+commands[docker] ))"
zplug "plugins/git", from:oh-my-zsh, if:"(( $+commands[git] ))"
zplug "plugins/aws", from:oh-my-zsh, if:"(( $+commands[aws] ))"
#zplug "plugins/pyenv", from:oh-my-zsh, if:"(( $+commands[pyenv] ))"
zplug "plugins/fasd", from:oh-my-zsh, on:"clvv/fasd"

zplug "andsens/homeshick", dir:$HOME/.homesick/repos/homeshick, use:homeshick.sh of:completions

zplug "zsh-users/zsh-history-substring-search", on:"zsh-users/zsh-syntax-highlighting", defer:3
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down

if ! zplug check --verbose; then
	echo "Install? [Yes/No]"
	select yn in "Yes" "No"; do
	    case $yn in
		Yes ) zplug install; break;;
		No ) exit;;
	    esac
	done
fi

zplug load
#if [ /usr/local/bin/kubectl ]; then source <(kubectl completion zsh); fi
