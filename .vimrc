let skip_defaults_vim=1
filetype off
syntax on

set background=dark
set backspace=2
set encoding=utf-8
set expandtab
set incsearch
set noautoindent
set nocompatible
set ruler
set shiftwidth=4
set softtabstop=4
set t_md= " disable ALL bold
set tabstop=4
set textwidth=72
set title
set titleold=
set viminfo^=h

" don't want to see these TYVM.
set viminfo+=n$XDG_DATA_HOME/viminfo
set backupdir=$XDG_CACHE_HOME
set directory=$XDG_CACHE_HOME

highlight Comment cterm=italic ctermfg=140
highlight Conditional cterm=italic ctermfg=202
highlight String cterm=italic ctermfg=217

highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/

" one of (harfbuzz||freetype||fontconfig) can't deal with italic emoji
highlight NonASCII cterm=nocombine term=nocombine gui=nocombine
match NonASCII "[^\x00-\x7F]"

autocmd Filetype c,h,cpp,hpp setl noet ts=8 sts=8 sw=8 tw=79
autocmd Filetype python setl tw=79

" see :help xterm-bracketed-paste
if &term =~ "tmux"
    let &t_BE = "\e[?2004h"
    let &t_BD = "\e[?2004l"
    exec "set t_PS=\e[200~"
    exec "set t_PE=\e[201~"
endif

set formatoptions=cq
