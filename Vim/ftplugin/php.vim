" Vim configuration for PHP
" Wouter Bolsterlee <uws@xs4all.nl>


setlocal textwidth=80
setlocal tabstop=4 shiftwidth=4
setlocal formatoptions=croqnl1
setlocal include=^\s\+(require\|require_once\|include\|include_once)
setlocal tags=tags;/

" Make sure the global ftplugin/php.vim doesn't add back the $ character to
" iskeyword setting, without using a ~/vim/after/ftplugin/php.vim file.
autocmd Filetype php setlocal iskeyword-=$

" Indentation
setlocal cindent
setlocal indentexpr=
filetype indent off

" Completion
setlocal complete+=k
setlocal dictionary+=~/.vim/extra/php-functionlist

" Folding
let php_folding=0
setlocal foldlevel=1
setlocal foldmethod=indent
setlocal nofoldenable

" Open the manual for the current keyword
if (exists("$GNOME_DESKTOP_SESSION_ID"))
	nnoremap <buffer> K :!gnome-open http://www.php.net/<C-R><C-W><Cr><Cr>
endif

" Check the current file for syntax errors
nnoremap <buffer> <F9> :w !php -l<CR>

" Adds different types of quotes around the current word
nnoremap <buffer> ,` mxgewi`<Esc>ea`<Esc>`xl
nnoremap <buffer> ,' mxgewi'<Esc>ea'<Esc>`xl
nnoremap <buffer> ," mxgewi"<Esc>ea"<Esc>`xl

" Quickly jump to the next variable
inoremap <buffer> <C-Enter> <Esc>f$a

" Abbreviations for control structures
inoreabbrev <buffer> if() if () {<CR>}<Up><C-O>f)<C-R>=EatWhitespace()<CR>
inoreabbrev <buffer> for() for ()<CR>{<CR>}<Up><Up><C-O>f)<C-R>=EatWhitespace()<CR>
inoreabbrev <buffer> while() while ()<Cr>{<CR>}<Up><Up><C-O>f)<C-R>=EatWhitespace()<CR>
inoreabbrev <buffer> foreach() foreach ($ as $)<CR>{<CR>}<Up><Up><C-O>2f <C-R>=EatWhitespace()<CR>
inoreabbrev <buffer> switch() switch ($)<Cr>{<CR>case FIXME:<CR>break;<CR><CR>default:<CR>break;<CR>}<Up><Up><Up><Up><Up><Up><Up><C-O>f)<C-R>=EatWhitespace()<CR>
inoreabbrev <buffer> fun function ()<Cr>{<CR>}<Up><Up><C-O>f(<C-R>=EatWhitespace()<CR>

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
