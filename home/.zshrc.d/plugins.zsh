if [[ ! -d ~/.zplug ]]; then
	git clone https://github.com/zplug/zplug ~/.zplug
	source ~/.zplug/init.zsh && zplug update
fi

source ~/.zplug/init.zsh

zplug "andsens/homeshick", dir:$HOME/.homesick/repos/homeshick, use:homeshick.sh
zplug "~/.zshrc.d/", as:theme, from:local, use:my.zshtheme
zplug "zsh-users/zsh-syntax-highlighting", defer:2
zplug "clvv/fasd", as:command, use:fasd
zplug "plugins/fasd", from:oh-my-zsh, on:"clvv/fasd"
zplug "plugins/docker", from:oh-my-zsh, if:"(( $+commands[docker] ))"
zplug "plugins/git", from:oh-my-zsh, if:"(( $+commands[git] ))"
zplug "plugins/aws", from:oh-my-zsh, if:"(( $+commands[aws] ))", defer:2
zplug "plugins/helm", from:oh-my-zsh, if:"(( $+commands[helm] ))", defer:2
zplug "plugins/kubectl", from:oh-my-zsh, if:"(( $+commands[kubectl] ))", defer:2
#zplug "plugins/pyenv", from:oh-my-zsh, if:"(( $+commands[pyenv] ))"


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
