" Vim configuration for Python

setlocal tabstop=4 shiftwidth=4 expandtab smarttab
setlocal foldmethod=indent

" Fix comments
inoremap <buffer> # X#

" PEP8 checker
setlocal makeprg=pep8\ --repeat\ --ignore=E501\ %

" Abbreviations
inoreabbrev <buffer> ifmain if __name__ == '__main__':<Cr><C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> defm def (self):<C-O>F(<C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> deff def ():<C-O>F(<C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> trye try:<Cr><C-D>except SomeError:<Cr>pass<Esc>kO<C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> tryf try:<Cr><C-D>finally:<Cr>pass<Esc>kO<C-R>=EatWhitespace()<Cr>
