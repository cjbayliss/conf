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

highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/

" a pleasant colour scheme based on 'ron'
colorschem ron
highlight Boolean cterm=none ctermbg=none ctermfg=183
highlight Comment ctermbg=none cterm=italic ctermfg=157
highlight Conditional cterm=italic ctermbg=none ctermfg=166
highlight Constant cterm=none ctermbg=none ctermfg=209
highlight CursorLine cterm=none ctermbg=234
highlight CursorLineNr cterm=none ctermbg=234 ctermfg=250
highlight Function ctermbg=none ctermfg=none cterm=italic,underline
highlight Identifier cterm=italic ctermbg=none ctermfg=180
highlight Include cterm=none ctermbg=none ctermfg=211
highlight LineNr cterm=none ctermfg=245
highlight Statement cterm=none ctermbg=none ctermfg=222
highlight String cterm=none ctermbg=none ctermfg=209
highlight Type cterm=none ctermbg=none ctermfg=167

" match non-ASCII and turn off bold/italics/underline because some
" terminals are bad.
highlight NonASCII cterm=nocombine term=nocombine gui=nocombine
match NonASCII "[^\x00-\x7F]"

" disable and fix some PHP syntax highlighting
highlight link phpDefine Statement
highlight link phpFunctions Boolean
highlight link phpIdentifier Normal
highlight link phpParent Normal
highlight link phpVarSelector Normal

highlight link pythonBuiltin Boolean

autocmd Filetype c,h,cpp,hpp setl noet ts=8 sts=8 sw=8 tw=79
autocmd Filetype python setl tw=79

set formatoptions=cq
