let skip_defaults_vim=1
filetype off
syntax on

set background=dark
set backspace=2
set encoding=utf-8
set expandtab
set noautoindent
set nocompatible
set ruler
set shiftwidth=4
set softtabstop=4
set tabstop=4
set textwidth=72
set viminfo=""
set t_md= " disable ALL bold

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

set formatoptions=cq
