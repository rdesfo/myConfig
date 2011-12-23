" color scheme of the moment:
syntax on
colorscheme torte

" REQUIRED. This makes vim invoke Latex-Suite when you open a tex file.
filetype plugin on

" IMPORTANT: win32 users will need to have 'shellslash' set so that latex
" can be called correctly.
" set shellslash

" IMPORTANT: grep will sometimes skip displaying the file name if you
" search in a singe file. This will confuse Latex-Suite. Set your grep
" program to always generate a file-name.
set grepprg=grep\ -nH\ $*

" OPTIONAL: This enables automatic indentation as you type.
filetype indent off

" OPTIONAL: Starting with Vim 7, the filetype of empty .tex files defaults to
" 'plaintex' instead of 'tex', which results in vim-latex not being loaded.
" The following changes the default filetype back to 'tex':
let g:tex_flavor='latex'
"let g:Tex_CompileRule_dvi='latex -src-specials -interaction=nonstopmode $*'
"let g:Tex_ViewRule_dvi = 'xdvik'
let g:Tex_DefaultTargetFormat = 'pdf'
let g:Tex_ViewRule_pdf = 'open -a Preview.app'
autocmd Filetype tex imap <C-i> <Plug>Tex_InsertItemOnThisLine
"let g:Tex_ItemStyle_list = '\item <++>'

"Gist.vim token
let g:github_user = 'rdesfo'
let g:github_token = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx'

"Do Math in Vim
"http://vim.wikia.com/wiki/Calculator_and_code_evaluation_using_Perl
:command! -nargs=+ Calc :perl VIM::Msg(eval{<args>})

" Move swap file to tmp dir
set swapfile
set dir=~/tmp
