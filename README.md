# installation

### 1. install zsh
### 2. execute
git clone git://github.com/andsens/homeshick.git $HOME/.homesick/repos/homeshick
$HOME/.homesick/repos/homeshick/bin/homeshick clone Lubricy/dotfiles
$HOME/.homesick/repos/homeshick/bin/homeshick clone hlissner/doom-emacs
cp ~/.emacs.d/init.example.el ~/.emacs.d/init.el
cd ~/.emacs.d && (yes | make) && cd -
### or
### curl -L https://raw.githubusercontent.com/Lubricy/dotfiles/master/README.md | bash

