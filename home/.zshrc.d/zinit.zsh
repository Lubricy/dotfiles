### Added by Zinit's installer
if [[ ! -f $HOME/.local/share/zinit/zinit.git/zinit.zsh ]]; then
    print -P "%F{33} %F{220}Installing %F{33}ZDHARMA-CONTINUUM%F{220} Initiative Plugin Manager (%F{33}zdharma-continuum/zinit%F{220})…%f"
    command mkdir -p "$HOME/.local/share/zinit" && command chmod g-rwX "$HOME/.local/share/zinit"
    command git clone https://github.com/zdharma-continuum/zinit "$HOME/.local/share/zinit/zinit.git" && \
        print -P "%F{33} %F{34}Installation successful.%f%b" || \
        print -P "%F{160} The clone has failed.%f%b"
fi

source "$HOME/.local/share/zinit/zinit.git/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit
### End of Zinit's installer chunk

setopt promptsubst
autoload -Uz colors; colors

zinit wait"!" lucid for \
    is-snippet \
        $HOME/.zshrc.d/my.zshtheme \

zinit lucid as"program" for \
    pick"fasd" \
        clvv/fasd \
    pick"bin/asdf" src"asdf.sh" \
        @asdf-vm/asdf \
    pick"bin/(fzf|fzf-tmux)" \
    atclone"cp shell/completion.zsh _fzf_completion; \
      cp bin/(fzf|fzf-tmux) $ZPFX/bin" \
    make"PREFIX=$ZPFX install" \
        junegunn/fzf \
    pick"bin/(fzf|fzf-tmux)" \
    atclone"cp shell/completion.zsh _fzf_completion; \
      cp bin/(fzf|fzf-tmux) $ZPFX/bin" \
    make"PREFIX=$ZPFX install" \
        junegunn/fzf \
    make'!' atclone'./direnv hook zsh > zhook.zsh' \
    atpull'%atclone' pick"direnv" src"zhook.zsh" \
        direnv/direnv \
    pick"git-open" \
        paulirish/git-open

# OMZ Plugins
zinit wait light-mode lucid blockf for \
        $HOME/.homesick/repos/homeshick \
        OMZL::git.zsh \
    atload"unalias grv" \
        OMZP::git \
    has'kubectl' \
        OMZP::kubectl \
    has'fasd' \
        OMZP::fasd \
    has'aws' \
        OMZP::aws \
    has'helm' \
        OMZP::helm \
    has'direnv' \
        OMZP::direnv \
    has'pyenv' \
        OMZP::pyenv \
    has'pipenv' \
        OMZP::pipenv \
    has'poetry' \
        OMZP::poetry \
    has'pipx' \
        thuandt/zsh-pipx \
    has'fzf' \
        Aloxaf/fzf-tab 

zinit wait lucid as'completion' for \
    is-snippet \
        $HOME/.homesick/repos/homeshick/completions/_homeshick \
    is-snippet \
        OMZ::plugins/docker/_docker \
    is-snippet \
        OMZ::plugins/pass/_pass \


zinit wait lucid blockf for\
    atpull'zinit creinstall -q .' \
        zsh-users/zsh-completions \
    atload'__bind_history_keys' \
        zsh-users/zsh-history-substring-search


zinit wait lucid for \
    atinit"ZINIT[COMPINIT_OPTS]=-C; zicompinit; zicdreplay" \
        zsh-users/zsh-syntax-highlighting \
        dracula/zsh-syntax-highlighting

