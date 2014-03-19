" Vim configuration for ReStructured TExt

setlocal tabstop=4 shiftwidth=4 expandtab
setlocal textwidth=80

" Special characters. Use triple backtick to avoid clashing with the
" ``literal`` syntax.
inoreabbrev <buffer> ... …
inoremap <buffer> ``` ‘
inoremap <buffer> '' ’

" Underline current line
noremap <Leader>= mxyypVr=`x
noremap <Leader>- mxyypVr-`x
noremap <Leader>* mxyypVr*`x
noremap <Leader>^ mxyypVr^`x
noremap <Leader>+ mxyypVr+`x
noremap <Leader># mxyypVr#`x
noremap <Leader>u yypVr
