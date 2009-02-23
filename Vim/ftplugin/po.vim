" Vim configuration for PO-style translation files
" Wouter Bolsterlee <uws@xs4all.nl>

inoremap <buffer> ... …
inoremap <buffer> `` ‘
inoremap <buffer> '' ’

noremap <buffer> <F6> :FuzzyDel<CR>
noremap <buffer> <F7> /, fuzzy<CR>/msgstr\(\[\d\]\)\? "/e+1<CR>:nohlsearch<CR>
noremap <buffer> <F8> /\([^\\]\\|^\)""\n\n/b+2<CR>:nohlsearch<CR>
noremap <buffer> <F9> :!msgfmt --check --statistics -o /dev/null %<CR>
noremap <buffer> <F10> ?^msgid<CR>V/msgstr<CR>kyjpkdd:s/^msgid/msgstr/<CR>:nohlsearch<CR>

function! RevisionDate()
  /"PO-Revision-Date: 
  normal ddk
  read!date '+"PO-Revision-Date: \%Y-\%m-\%d \%H:\%M\%z\n"'
endfunction
command! RevisionDate call RevisionDate()

function! LastTranslator()
  /"Last-Translator: /
  normal dd
  normal O"Last-Translator: Wouter Bolsterlee <wbolster@gnome.org>\n"
  normal 0
endfunction
command! LastTranslator call LastTranslator()

function! FuzzyDel()
   ?^msgid "?
   normal k
   let line = getline(".")
   if line =~ '^#, fuzzy$'
     exe "normal! dd"
   else
     exe 's/, fuzzy//'
   endif
   normal }
endfunction
command! FuzzyDel call FuzzyDel()
