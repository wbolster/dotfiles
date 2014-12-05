" Vim configuration for Python

setlocal tabstop=4 shiftwidth=4 expandtab smarttab
setlocal foldmethod=indent
setlocal formatoptions=croqnl1

" Fix comments dedenting
inoremap <buffer> # X#

" Source code checking (flake8, pyflakes, pep8)
setlocal makeprg=pep8\ --repeat\ --ignore=E501\ %
let g:syntastic_python_checker_args = "--ignore=E501"

" Debugging
inoreabbrev <buffer> pdb import pdb<Return>pdb.set_trace()
inoreabbrev <buffer> ipdb import ipdb<Return>ipdb.set_trace()
inoreabbrev <buffer> ipy import IPython<Return>IPython.embed()
