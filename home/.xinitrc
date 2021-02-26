#!/bin/sh

xrdb -merge ~/.Xresources
dunst &
picom &
~/.fehbg &
export PATH=$HOME/.local/bin:$HOME/.asdf/shims:$PATH
exec xmonad
