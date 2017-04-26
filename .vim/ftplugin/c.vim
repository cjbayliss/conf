set tabstop=8
set shiftwidth=8
set noexpandtab

" NOTE: The folowing is from part of vim-syntax-extra:
" https://github.com/justinmk/vim-syntax-extra/blob/master/after/syntax/c.vim

" Operators
syn match cOperator   "\(<<\|>>\|[-+*/%&^|<>!=]\)="
syn match cOperator   "<<\|>>\|&&\|||\|++\|--\|->"
syn match cOperator   "[.!~*&%<>^|=,+-]"
syn match cOperator   "/[^/*=]"me=e-1
syn match cOperator   "/$"
syn match cOperator "&&\|||"
syn match cOperator   "[][]"

" Preprocs
syn keyword cDefined defined contained containedin=cDefine
hi def link cDefined cDefine

" Functions
syn match cUserFunction "\<\h\w*\>\(\s\|\n\)*("me=e-1 contains=cType,cDelimiter,cDefine
syn match cUserFunctionPointer "(\s*\*\s*\h\w*\s*)\(\s\|\n\)*(" contains=cDelimiter,cOperator

hi def link cUserFunction cFunction
hi def link cUserFunctionPointer cFunction

" Delimiters
syn match cDelimiter    "[();\\]"
" foldmethod=syntax fix, courtesy of Ivan Freitas
syn match cBraces display "[{}]"

" Booleans
syn keyword cBoolean true false TRUE FALSE

" Links
hi def link cFunction Function
hi def link cIdentifier Identifier
hi def link cDelimiter Delimiter
" foldmethod=syntax fix, courtesy of Ivan Freitas
hi def link cBraces Delimiter
hi def link cBoolean Boolean

