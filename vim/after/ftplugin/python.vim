setlocal smarttab
setlocal foldmethod=indent
setlocal formatoptions=croqnl1

" Fix comments dedenting
inoremap <buffer> # X#

" Allow proper formatting for comments starting with #: (used by Sphinx for
" documenting class attributes).
setlocal comments+=b:#:

" Quick placeholders
inoreabbrev <buffer> ... ...  # TODO
inoreabbrev <buffer> rnie raise NotImplementedError
