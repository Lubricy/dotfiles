set nocompatible
filetype plugin on
set clipboard=unnamedplus
set shell=bash
let mapleader = "\<Space>"

let mapleader=" "

"The sudo tewak
command! W :execute ':silent w !sudo tee % > /dev/null' | :edit!

"Misc
syntax on
set number ruler
set background=light
set undofile
set undodir=~/.config/vim/undo
set wrap
set linebreak
set tabstop=2 expandtab shiftwidth=2 softtabstop=2
set hlsearch incsearch
set ignorecase
set smartcase
set smarttab
set smartindent
set showcmd
set pastetoggle=<F10>
set foldmethod=marker
if &diff
  set diffopt-=internal
  set diffopt+=vertical
endif

nnoremap <Up> gk
nnoremap <Down> gj
nnoremap <silent> <Leader><Up> :wincmd k<CR>
nnoremap <silent> <Leader><Down> :wincmd j<CR>
nnoremap <silent> <Leader><Left> :wincmd h<CR>
nnoremap <silent> <Leader><Right> :wincmd l<CR>

set backspace=indent,eol,start

