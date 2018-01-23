set nocompatible
filetype plugin on
set shell=/bin/bash
let mapleader = "\<Space>"

if has("nvim")
  let $NVIM_TUI_ENABLE_TRUE_COLOR=1
  let $NVIM_TUI_ENABLE_CURSOR_SHAPE=0
  set termguicolors
  set clipboard+=unnamed
end

source ~/.vim/plugins.vim
source ~/.vim/colors.vim

let mapleader=" "

"The sudo tewak
command! W :execute ':silent w !sudo tee % > /dev/null' | :edit!

"auto append `gf` suffixes by filetype
augroup suffixes
  autocmd!
  let associations = [
        \["javascript", ".js,.javascript,.es,.esx,.json"],
        \["typescript", ".ts,.jsx,.js,.javascript,.es,.esx,.json"],
        \["python", ".py,.pyw"]
        \]

  for ft in associations
    execute "autocmd FileType " . ft[0] . " setlocal suffixesadd=" . ft[1]
  endfor
augroup END

"Spell check settings
"hi clear SpellBad
"hi SpellBad cterm=underline

" Quickly opereload vim
nnoremap <leader>rc :split $MYVIMRC<CR>
nnoremap <leader>rr :source $MYVIMRC<CR>
if has ('autocmd') " Remain compatible with earlier versions
  augroup vimrc     " Source vim configuration upon save
    autocmd!
    autocmd BufWritePost $MYVIMRC source % | echom "Reloaded " . $MYVIMRC | redraw
    autocmd BufWritePost $MYGVIMRC if has('gui_running') | so % | echom "Reloaded " . $MYGVIMRC | endif | redraw
  augroup END
endif " has autocmd

"Status Line
set laststatus=2
set statusline=%-28.28F%4(\ %m%)
set statusline+=%#moremsg#%{fugitive#statusline()}%*
set statusline+=%=
set statusline+=%{ALEGetStatusLine()}\ \ \ \ 
set statusline+=%-14.(%l,%c%V%)\ %P\ %h%w%q

" "Netrw
" let g:netrw_liststyle=3         " thin (change to 3 for tree)
" let g:netrw_banner=0            " no banner
" let g:netrw_altv=1              " open files on right
" let g:netrw_preview=1           " open previews vertically
" fun! VexToggle(dir)
"   if exists("t:vex_buf_nr")
"     call VexClose()
"   else
"     call VexOpen(a:dir)
"   endif
" endf

" fun! VexOpen(dir)
"   let g:netrw_browse_split=4    " open files in previous window
"   let vex_width = 25

"   execute "Vexplore " . a:dir
"   let t:vex_buf_nr = bufnr("%")
"   wincmd H

"   call VexSize(vex_width)
" endf
" noremap <Leader>n :call VexToggle(getcwd())<CR>
" noremap <Leader>N :call VexToggle("")<CR>

" fun! VexClose()
"   let cur_win_nr = winnr()
"   let target_nr = ( cur_win_nr == 1 ? winnr("#") : cur_win_nr )

"   1wincmd w
"   close
"   unlet t:vex_buf_nr

"   execute (target_nr - 1) . "wincmd w"
"   call NormalizeWidths()
" endf
" fun! VexSize(vex_width)
"   execute "vertical resize" . a:vex_width
"   set winfixwidth
"   call NormalizeWidths()
" endf

" fun! NormalizeWidths()
"   let eadir_pref = &eadirection
"   set eadirection=hor
"   set equalalways! equalalways!
"   let &eadirection = eadir_pref
" endf

" augroup NetrwGroup
"   autocmd! BufEnter * call NormalizeWidths()
" augroup END


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
set diffopt+=vertical

nnoremap g2 :diffget //2<CR>
nnoremap g3 :diffget //3<CR>

nnoremap ; :
nnoremap <F8> :let &so=999-&so<CR>:echo 'autocenter'&so?'on':'off'<CR>
nnoremap <Up> gk
nnoremap <Down> gj
nnoremap <silent> <Leader><Up> :wincmd k<CR>
nnoremap <silent> <Leader><Down> :wincmd j<CR>
nnoremap <silent> <Leader><Left> :wincmd h<CR>
nnoremap <silent> <Leader><Right> :wincmd l<CR>


set exrc
set secure
set backspace=indent,eol,start
set fileencodings=ucs-bom,utf-8,cp936,gb18030,big5,euc-jp,euc-kr,latin1

