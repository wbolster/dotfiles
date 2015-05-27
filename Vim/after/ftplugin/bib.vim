" Vim configuration for BibTeX

setlocal textwidth=0
setlocal shiftwidth=2
setlocal tabstop=2
setlocal expandtab
setlocal foldmethod=indent


" Abbreviations for entries

inoreabbrev <buffer> @a @article{,<CR>}<Esc>k0f,i<C-R>=EatWhitespace()<CR>
inoreabbrev <buffer> @b @book{,<CR>}<Esc>k0f,i<C-R>=EatWhitespace()<CR>
inoreabbrev <buffer> @i @inproceedings{,<CR>}<Esc>k0f,i<C-R>=EatWhitespace()<CR>
inoreabbrev <buffer> @m @misc{,<CR>}<Esc>k0f,i<C-R>=EatWhitespace()<CR>


" Abbreviations for properties

inoreabbrev <buffer> address= address= {{}},<Left><Left><Left><C-R>=EatWhitespace()<CR>
inoreabbrev <buffer> author= author= {{}},<Left><Left><Left><C-R>=EatWhitespace()<CR>
inoreabbrev <buffer> booktitle= booktitle= {{}},<Left><Left><Left><C-R>=EatWhitespace()<CR>
inoreabbrev <buffer> editor= editor= {{}},<Left><Left><Left><C-R>=EatWhitespace()<CR>
inoreabbrev <buffer> isbn= isbn= {},<Left><Left><C-R>=EatWhitespace()<CR>
inoreabbrev <buffer> journal= journal= {{}},<Left><Left><Left><C-R>=EatWhitespace()<CR>
inoreabbrev <buffer> location= location= {{}},<Left><Left><Left><C-R>=EatWhitespace()<CR>
inoreabbrev <buffer> pages= pages= {},<Left><Left><C-R>=EatWhitespace()<CR>
inoreabbrev <buffer> publisher= publisher= {{}},<Left><Left><Left><C-R>=EatWhitespace()<CR>
inoreabbrev <buffer> title= title= {{}},<Left><Left><Left><C-R>=EatWhitespace()<CR>
inoreabbrev <buffer> url= url= {},<Left><Left><C-R>=EatWhitespace()<CR>
inoreabbrev <buffer> year= year= {},<Left><Left><C-R>=EatWhitespace()<CR>
