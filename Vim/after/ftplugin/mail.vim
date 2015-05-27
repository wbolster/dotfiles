" Vim configuration for mail

setlocal textwidth=72
setlocal tabstop=2 shiftwidth=2 expandtab

" Delete empty quotes lines
map <buffer> ,> mx:g/^\(>\s*\)\+\s*$/d<CR>`x
