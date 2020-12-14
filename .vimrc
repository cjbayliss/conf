let skip_defaults_vim=1
filetype off
syntax on

set noautoindent
set background=dark
set backspace=2
set encoding=utf-8
set expandtab
set nocompatible
set ruler
set shiftwidth=4
set softtabstop=4
set tabstop=4
set textwidth=72
set viminfo=""

highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/

autocmd Filetype c,h,cpp,hpp setl noet ts=8 sts=8 sw=8 tw=79
autocmd Filetype python setl tw=79

set formatoptions=cq
