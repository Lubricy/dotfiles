# installation

### 1. install zsh

### 2. execute

git clone git://github.com/andsens/homeshick.git $HOME/.homesick/repos/homeshick
$HOME/.homesick/repos/homeshick/bin/homeshick clone Lubricy/dotfiles
if command -v emacs; then
$HOME/.homesick/repos/homeshick/bin/homeshick clone hlissner/doom-emacs
fi
if command -v nvim; then
$HOME/.homesick/repos/homeshick/bin/homeshick clone NTBBloodbath/doom-nvim
fi

### or

### curl -L https://raw.githubusercontent.com/Lubricy/dotfiles/master/README.md | bash
