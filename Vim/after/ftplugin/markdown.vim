" Vim configuration for Markdown

setlocal textwidth=9999999
setlocal tabstop=2 shiftwidth=2 expandtab
setlocal spell
setlocal nosmartindent
setlocal cinwords=

" Simplify long line handling
noremap <buffer> j gj
noremap <buffer> k gk
noremap <buffer> 0 g0
noremap <buffer> $ g$

" Special characters.
inoreabbrev <buffer> ... …
inoremap <buffer> `' ‘’<Left>

" Additional vim-surround mapping for code blocks. This uses the c key (short
" for code) for the replacement, e.g. typing ysiwc surrounds the current word
" with triple backticks.
let b:surround_99 = "```\r```"
