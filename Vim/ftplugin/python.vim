" Vim configuration for Python
" Wouter Bolsterlee <uws@xs4all.nl>

setlocal tabstop=4 shiftwidth=4 expandtab
setlocal smarttab

setlocal foldmethod=indent
setlocal nofoldenable

setlocal tags=tags;/

" Fix comments
inoremap <buffer> # X#

" Highlight leading tabs in the source code
match Todo /^\(\s\)*\t\+/

" PEP8 checker
setlocal makeprg=pep8\ --repeat\ %

" Abbreviations
inoreabbrev <buffer> ifmain if __name__ == '__main__':<Cr>
inoreabbrev <buffer> defm def (self):<C-O>F(<C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> deff def ():<C-O>F(<C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> trye try:<Cr><C-D>except SomeError:<Cr>pass<Esc>kO<C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> tryf try:<Cr><C-D>finally:<Cr>pass<Esc>kO<C-R>=EatWhitespace()<Cr>
