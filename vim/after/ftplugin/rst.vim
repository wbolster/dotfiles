" Vim configuration for ReStructured TExt

setlocal tabstop=2 shiftwidth=2 expandtab
setlocal textwidth=80
setlocal spell
autocmd Filetype rst setlocal comments=b:..  " overwrite system setting

" Special characters. Use triple backtick to avoid clashing with the
" ``literal`` syntax.
inoreabbrev <buffer> ... …
inoremap <buffer> ``` ‘
inoremap <buffer> '' ’

" Admonitions
inoreabbrev <buffer> ..n .. note::<Enter><Enter>   <C-R>=EatWhitespace()<Enter>
inoreabbrev <buffer> ..t .. tip::<Enter><Enter>   <C-R>=EatWhitespace()<Enter>
inoreabbrev <buffer> ..w .. warning::<Enter><Enter>   <C-R>=EatWhitespace()<Enter>

" Underline current line
noremap <buffer> <Leader>= mxyypVr=`x
noremap <buffer> <Leader>- mxyypVr-`x
noremap <buffer> <Leader>* mxyypVr*`x
noremap <buffer> <Leader>^ mxyypVr^`x
noremap <buffer> <Leader>+ mxyypVr+`x
noremap <buffer> <Leader># mxyypVr#`x
noremap <buffer> <Leader>u yypVr

" Additional vim-surround mapping for code blocks. This uses the c key (short
" for code) for the replacement, e.g. typing ysiwc surrounds the current word
" with double backticks.
let b:surround_99 = "``\r``"
