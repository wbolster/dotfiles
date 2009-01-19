" Vim configuration for PHP
" Wouter Bolsterlee <uws@xs4all.nl>


setlocal cindent
setlocal indentexpr=
setlocal complete+=k
setlocal dictionary+=~/.vim/extra/php-functionlist
setlocal foldlevel=1
setlocal foldmethod=indent
setlocal formatoptions=croqnl1
setlocal include=^\s\+(require\|require_once\|include\|include_once)
setlocal nofoldenable
setlocal tags=tags;/
setlocal tabstop=4
setlocal shiftwidth=4
setlocal textwidth=80

" No stupid indenting
filetype indent off

" No smart folding
let php_folding=0

" show the manual for the current keyword
nnoremap <buffer> K :!gnome-open http://www.php.net/<C-R><C-W><Cr><Cr>

" Check the current file for syntax errors
nnoremap <buffer> <F9> :w !php -l<CR>

" Adds different types of quotes around the current word
nnoremap <buffer> ,` mxgewi`<Esc>ea`<Esc>`xl
nnoremap <buffer> ,' mxgewi'<Esc>ea'<Esc>`xl
nnoremap <buffer> ," mxgewi"<Esc>ea"<Esc>`xl

" Abbreviations for control structures
inoreabbrev <buffer> if() if () {<CR>}<Up><C-O>f)<C-R>=EatWhitespace()<CR>
inoreabbrev <buffer> for() for () {<CR>}<Up><C-O>f)<C-R>=EatWhitespace()<CR>
inoreabbrev <buffer> while() while () {<CR>}<Up><C-O>f)<C-R>=EatWhitespace()<CR>
inoreabbrev <buffer> foreach() foreach ($ as $) {<CR>}<Up><C-O>2f <C-R>=EatWhitespace()<CR>
inoreabbrev <buffer> switch() switch ($) {<CR>case FIXME:<CR>break;<CR><CR>default:<CR>break;<CR>}<Up><Up><Up><Up><Up><Up><C-O>f)<C-R>=EatWhitespace()<CR>
inoreabbrev <buffer> fun function () {<CR>}<Up><C-O>f(<C-R>=EatWhitespace()<CR>

" Abbreviations for debugging
inoreabbrev <buffer> vd var_dump($);<Left><Left><C-R>=EatWhitespace()<CR>
inoreabbrev <buffer> pr print_r($);<Left><Left><C-R>=EatWhitespace()<CR>

" Abbreviations for globals
inoreabbrev <buffer> _C _COOKIE['']<Left><Left><C-R>=EatWhitespace()<CR>
inoreabbrev <buffer> _F _FILES['']<Left><Left><C-R>=EatWhitespace()<CR>
inoreabbrev <buffer> _G _GET['']<Left><Left><C-R>=EatWhitespace()<CR>
inoreabbrev <buffer> _P _POST['']<Left><Left><C-R>=EatWhitespace()<CR>
inoreabbrev <buffer> _S _SERVER['']<Left><Left><C-R>=EatWhitespace()<CR>

" Abbreviations for i18n
inoremap <buffer> _<C-K> _('')<Left><Left>
inoremap <buffer> N_<C-K> N_('')<Left><Left>
inoremap <buffer> Q_<C-K> Q_('\|')<Left><Left><Left>
