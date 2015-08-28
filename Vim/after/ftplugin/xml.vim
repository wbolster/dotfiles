" Vim configuration for XML

setlocal shiftwidth=2 tabstop=2 expandtab
setlocal foldmethod=indent nofoldenable
setlocal formatprg=xmllint\ --format\ -
setlocal matchpairs+=<:>
setlocal iskeyword+=: " useful for namespace prefixes
setlocal iskeyword+=- " useful for xslt

" quickly jump outside the current tag
inoremap <C-CR> <C-O>f><Right>

" simple tag
inoremap <buffer> <C-K> <Esc>"xyiwi<<Esc>ea></<C-R>x><C-O>F<

" simple tag with attribute
inoremap <buffer> <C-K><C-K> <Esc>"xyiwi<<Esc>ea ></<C-R>x><C-O>F>

" container tag
inoremap <buffer> <C-K><C-K><C-K> <Esc>"xyiwi<<Esc>ea><CR></<C-R>x><C-O>O<Tab>

" container tag with attribute
inoremap <buffer> <C-K><C-K><C-K><C-K> <Esc>"xyiwi<<Esc>ea ><CR></<C-R>x><C-O>O<Tab><C-O>k<C-O>f>
