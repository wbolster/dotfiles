" Vim configuration for mail
" Wouter Bolsterlee <uws@xs4all.nl>

setlocal textwidth=76
setlocal tabstop=2 shiftwidth=2 expandtab
setlocal equalprg=par\ h2\ w76\ j1

" Delete empty quotes lines
map <buffer> ,> mx:g/^\(>\s*\)\+\s*$/d<CR>`x
