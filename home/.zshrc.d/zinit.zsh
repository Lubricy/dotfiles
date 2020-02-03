if [[ ! -d $HOME/.zinit/bin ]]; then
  mkdir $HOME/.zinit
  git clone https://github.com/zdharma/zinit.git $HOME/.zinit/bin
  source $HOME/.zinit/bin/zinit.zsh
  zinit self-update
fi
source $HOME/.zinit/bin/zinit.zsh

zinit ice lucid blockf
zinit light $HOME/.homesick/repos/homeshick

zinit ice wait lucid as"completion"
zinit snippet $HOME/.homesick/repos/homeshick/completions/_homeshick

zinit ice lucid
zinit snippet OMZ::lib/git.zsh

zinit ice wait atload"unalias grv" lucid
zinit snippet OMZ::plugins/git/git.plugin.zsh

zinit ice wait lucid
zinit snippet OMZ::plugins/kubectl/kubectl.plugin.zsh

zinit ice wait lucid
zinit snippet OMZ::plugins/aws/aws.plugin.zsh

zinit ice wait lucid
zinit snippet OMZ::plugins/helm/helm.plugin.zsh

zinit ice wait"1" atload"zicompinit; zicdreplay -q" lucid
zinit snippet OMZ::plugins/pyenv/pyenv.plugin.zsh

zinit ice wait lucid as"completion"
zinit snippet OMZ::plugins/docker/_docker

zinit ice wait lucid blockf atpull'zinit creinstall -q .'
zplugin light zsh-users/zsh-completions

bindkey -r '^[[A'
bindkey -r '^[[B'
function __bind_history_keys() {
  bindkey '^[[A' history-substring-search-up
  bindkey '^[[B' history-substring-search-down
}
zinit ice wait lucid atload'__bind_history_keys'
zinit load zsh-users/zsh-history-substring-search


zinit ice lucid
zinit load zsh-users/zsh-syntax-highlighting

zinit ice lucid atload"_zsh_autosuggest_start"
zinit load zsh-users/zsh-autosuggestions

setopt promptsubst
autoload -Uz colors; colors
zinit ice lucid
zinit snippet $HOME/.zshrc.d/my.zshtheme

