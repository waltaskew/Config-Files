set nocompatible

syntax on

filetype on
filetype indent on
filetype plugin on

set incsearch
set nohlsearch

set showmatch

let loaded_matchparen = 1

set ruler
set showmode
set showcmd

set tabstop=4
set shiftwidth=4
set softtabstop=4

set smarttab
set expandtab
set autoindent

"use tabs for menu files
autocmd BufRead,BufNewFile *menu set noexpandtab

"paste toggle
set pastetoggle=<F2>

" Tell vim to remember certain things when we exit
"  '10  :  marks will be remembered for up to 10 previously edited files
"  "100 :  will save up to 100 lines for each register
"  :20  :  up to 20 lines of command-line history will be remembered
"  %    :  saves and restores the buffer list
"  n... :  where to save the viminfo files
set viminfo='10,\"100,:20,%,n~/.viminfo

" Return to the last cursor position when opening files.
function! ResCur()
  if line("'\"") <= line("$")
    normal! g`"
    return 1
  endif
endfunction

augroup resCur
  autocmd!
  autocmd BufWinEnter * call ResCur()
augroup END
