execute pathogen#infect()

let g:syntastic_python_checkers = ['flake8']
let g:syntastic_ruby_checkers = ['rubocop', 'mri', 'rubylint']
let g:syntastic_haml_checkers = ['haml_lint']
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_check_on_open = 1

set nocompatible

syntax on
set background=dark
colorscheme solarized

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

:imap jj <Esc>

"use tabs for menu files
autocmd BufRead,BufNewFile *menu set noexpandtab

"treat .json files as javascript
autocmd BufNewFile,BufRead *.json set ft=javascript
autocmd BufNewFile,BufRead *.json set tabstop=2
autocmd BufNewFile,BufRead *.json set shiftwidth=2
autocmd BufNewFile,BufRead *.json set softtabstop=2

"tabstop 2 on .yml
autocmd BufNewFile,BufRead *.yml set tabstop=2
autocmd BufNewFile,BufRead *.yml set shiftwidth=2
autocmd BufNewFile,BufRead *.yml set softtabstop=2

"tabstop 2 for javascript
autocmd BufNewFile,BufRead *.js set tabstop=2
autocmd BufNewFile,BufRead *.js set shiftwidth=2
autocmd BufNewFile,BufRead *.js set softtabstop=2

"markdown on .md files
augroup markdown
    au!
    au BufNewFile,BufRead *.md,*.markdown setlocal filetype=ghmarkdown
augroup END

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

" Add :Shell command to run commands and drop their output in a scratch buffer
function! s:ExecuteInShell(command)
  let command = join(map(split(a:command), 'expand(v:val)'))
  let winnr = bufwinnr('^' . command . '$')
  silent! execute  winnr < 0 ? 'botright new ' . fnameescape(command) : winnr . 'wincmd w'
  setlocal buftype=nowrite bufhidden=wipe nobuflisted noswapfile nowrap
  echo 'Execute ' . command . '...'
  silent! execute 'silent %!'. command
  silent! execute 'resize 12'
  silent! redraw
  silent! execute 'au BufUnload <buffer> execute bufwinnr(' . bufnr('#') . ') . ''wincmd w'''
  silent! execute 'nnoremap <silent> <buffer> <LocalLeader>r :call <SID>ExecuteInShell(''' . command . ''')<CR>'
  echo 'Shell command ' . command . ' executed.'
endfunction
command! -complete=shellcmd -nargs=+ Shell call s:ExecuteInShell(<q-args>)

function! FileName()
    return expand('%:p')
endfu

function! PathName()
    return expand('%:p:h')
endfu

command! -nargs=* Pylint call s:ExecuteInShell('pylint ' . FileName() . ' ' . <q-args>)
command! -nargs=* Python call s:ExecuteInShell('python ' . FileName() . ' ' . <q-args>)
