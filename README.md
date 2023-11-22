# installation

### 1. install zsh

### 2. execute

git clone git://github.com/andsens/homeshick.git $HOME/.homesick/repos/homeshick
$HOME/.homesick/repos/homeshick/bin/homeshick clone Lubricy/dotfiles
if command -v emacs; then
$HOME/.homesick/repos/homeshick/bin/homeshick clone doomemacs/doomemacs
fi
if command -v nvim; then
$HOME/.homesick/repos/homeshick/bin/homeshick clone LunarVim/LunarVim
fi

THEMES=(iterm sublime)

THEMES_DIR="$HOME/.homesick/repos/dracula-themes"
set +m
mkdir -p $THEMES_DIR
clone-theme () {
  t=$1
  echo "cloning dracula theme ${t}..." \
  && git -C "$THEMES_DIR" clone --quiet "https://github.com/dracula/${t}.git" --depth 1 \
 && echo "cloned dracula theme ${t}."
}

for t in ${THEMES[@]}; do
    clone-theme "$t" &
done
wait
set -m

### or

### curl -L https://raw.githubusercontent.com/Lubricy/dotfiles/master/README.md | bash
