" Vim configuration for JavaScript
" Wouter Bolsterlee <uws@xs4all.nl>

setlocal cindent
setlocal formatoptions=croqnl1

" Abbreviations
inoreabbrev <buffer> if() if () {<CR>}<Up><C-O>f)<C-R>=EatWhitespace()<CR>
inoreabbrev <buffer> for() for () {<CR>}<Up><C-O>f)<C-R>=EatWhitespace()<CR>
inoreabbrev <buffer> while() while () {<CR>}<Up><C-O>f)<C-R>=EatWhitespace()<CR>
inoreabbrev <buffer> fun function () {<CR>}<Up><C-O>f(<C-R>=EatWhitespace()<CR>
inoreabbrev <buffer> al alert();<Left><Left><C-R>=EatWhitespace()<CR>
