if empty(glob('~/.vim/dein/repos/github.com/Shougo/dein.vim'))
  silent !git clone https://github.com/Shougo/dein.vim.git ~/.vim/dein/repos/github.com/Shougo/dein.vim
    " autocmd VimEnter * call dein#install() | source $MYVIMRC
endif

set runtimepath+=~/.vim/dein/repos/github.com/Shougo/dein.vim

call dein#begin(expand('~/.vim/dein'))
call dein#add('Shougo/dein.vim')
" denite.nvim {{{
call dein#add('Shougo/denite.nvim')
call denite#custom#map(
	      \ 'insert',
	      \ '<Esc>',
	      \ '<denite:enter_mode:normal>',
	      \ 'noremap'
	      \)
call denite#custom#map(
	      \ 'insert',
	      \ '<Up>',
	      \ '<denite:move_to_previous_line>',
	      \ 'noremap'
	      \)
call denite#custom#map(
	      \ 'insert',
	      \ '<Down>',
	      \ '<denite:move_to_next_line>',
	      \ 'noremap'
	      \)
nnoremap <Leader>f :Denite grep:::<C-r>=expand("<cword>")<CR><cr>
vnoremap <Leader>f ay:<C-u>Denite grep:::<C-r>a<CR>
nnoremap <C-p> :Denite file_mru file_rec<cr>
" }}}
call dein#add('Shougo/neomru.vim')
"" NERDTree {{{
"call dein#add('scrooloose/nerdtree')
"nnoremap <Leader>n :NERDTreeToggle<CR>
""NERDTree goto file on open
"function! s:IfNERDTreeOpenGotoFile()
"  if exists("t:NERDTreeBufName")
"    if bufwinnr(t:NERDTreeBufName) != -1
"      if !empty(expand("%"))
"        NERDTreeFind
"      endif
"    endif
"  endif
"endfunction
"augroup NERDTree
"  autocmd!
"  autocmd BufWinEnter * call s:IfNERDTreeOpenGotoFile()
"  autocmd WinEnter * call s:CloseIfOnlyNerdTreeLeft()
"augroup end
"
"" Close all open buffers on entering a window if the only
"" buffer that's left is the NERDTree buffer
"function! s:CloseIfOnlyNerdTreeLeft()
"  if exists("t:NERDTreeBufName")
"    if bufwinnr(t:NERDTreeBufName) != -1
"      if winnr("$") == 1
"        q
"      endif
"    endif
"  endif
"endfunction
"" }}}
" ALE {{{
call dein#add('w0rp/ale')
let g:ale_change_sign_column_color = 1
let g:ale_sign_column_always = 1
let g:ale_set_signs = 0
" }}}


" vim-slime {{{
call dein#add('jpalardy/vim-slime')
let g:slime_target = "tmux"
let g:slime_paste_file = "$HOME/.slime_paste"
let g:slime_no_mappings = 1
let g:slime_dont_ask_default = 1
let g:slime_default_config = {"socket_name": "default", "target_pane": "1"}
xmap <Leader>t <Plug>SlimeRegionSend
nmap <Leader>t <Plug>SlimeParagraphSend
nmap <Leader>S <Plug>SlimeConfig
" }}}
" autocomplete
call dein#add('Shougo/deoplete.nvim')
let g:deoplete#enable_at_startup = 1

call dein#add('zchee/deoplete-zsh', { 'on_ft':['zsh']})
call dein#add('zchee/deoplete-jedi', { 'on_ft':['python']})

call dein#add('milkypostman/vim-togglelist')
call dein#add('bkad/CamelCaseMotion')
call dein#add('tpope/vim-fugitive')
call dein#add('int3/vim-extradite')
call dein#add('tpope/vim-git')
call dein#add('tpope/vim-commentary')
call dein#add('wellle/tmux-complete.vim')
call dein#add('Chiel92/vim-autoformat')
call dein#add('godlygeek/tabular')
call dein#add('gorodinskiy/vim-coloresque')
call dein#add('Konfekt/FastFold')
call dein#add('tmhedberg/matchit')
call dein#add('vim-scripts/surround.vim')
call dein#add('Raimondi/delimitMate')
call dein#add('tpope/vim-repeat')
call dein#add('yonchu/accelerated-smooth-scroll')
call dein#add('michaeljsmith/vim-indent-object')

call dein#add('kelwin/vim-smali' , { 'on_ft' : ['smali'] })
call dein#add('Rip-Rip/clang_complete' , { 'on_ft' : ['cpp'] })
call dein#add('dag/vim-fish' , { 'on_ft' : ['fish'] })
call dein#add('digitaltoad/vim-pug' , {'on_ft' : ['pug' ] })
call dein#add('neovimhaskell/haskell-vim' , { 'on_ft': ['haskell'] })
call dein#add( 'itchyny/vim-haskell-indent' , { 'on_ft': ['haskell'] })
call dein#add( 'eagletmt/neco-ghc', { 'on_ft': ['haskell'] })
call dein#add( 'leafgarland/typescript-vim' , {'on_ft' : 'typescript' })
call dein#add( 'othree/es.next.syntax.vim' , {'on_ft' : 'javascript'})
call dein#add( 'othree/yajs.vim' , {'on_ft' :[  'javascript' ]})
call dein#add( 'Quramy/tsuquyomi' , {'on_ft' :[  'typescript' ]} )
call dein#add( 'vim-scripts/ditaa' , { 'on_ft' : [ 'ditaa' ] })
call dein#add( 'vim-scripts/DrawIt' , { 'on_ft' : [ 'ditaa' ] })
call dein#add( 'tfnico/vim-gradle' , { 'on_ft' : [ 'groovy' ] })
call dein#add( 'wlangstroth/vim-racket' , {'on_ft' : ['lisp' ]})


call dein#add('SirVer/ultisnips')
call dein#add('honza/vim-snippets')

call dein#end()
call dein#local('~/.vim/local')
" call dein#install()

" Make sure you use single quotes
" Group dependencies, vim-snippets depends on ultisnips

" Side Panes
"
"" Git
"
"" Utils
"Plug 'critiqjo/lldb.nvim' , {'on' : 'LLsession'}
"Plug 'szw/vim-g' , {'on': 'Google'}
"Plug 'easymotion/vim-easymotion'
"
"" Autocomple
"Plug 'Valloric/YouCompleteMe', { 'do': 'python ./install.py --all'}
"Plug 'rdnetto/YCM-Generator', { 'branch': 'stable'}
"Plug 'SirVer/ultisnips', {'on': []} | Plug 'honza/vim-snippets'
"
"" Autocmd
"" Plug 'neomake/neomake'
"
"" Behaviour change
"Plug 'altercation/vim-colors-solarized'
"
"" language specific
"" Add plugins to &runtimepath
"call plug#end()
"
"augroup load_on_insert
"  autocmd!
"  autocmd InsertEnter * call plug#load('ultisnips')
"                     \| autocmd! load_on_insert
"augroup END
