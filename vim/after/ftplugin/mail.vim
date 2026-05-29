setlocal expandtab
setlocal shiftwidth=2
setlocal tabstop=2
setlocal textwidth=72

" Delete empty quotes lines
nnoremap <buffer> ,> mx:g/^\(>\s*\)\+\s*$/d<CR>`x
