#!/bin/sh

xrdb -merge ~/.Xresources
dunst &
picom &
~/.fehbg &
export PATH=$HOME/.local/bin:$HOME/.asdf/shims:$PATH
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx
fcitx -d &
exec xmonad
