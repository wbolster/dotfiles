" Vim configuration for PO translation files

setlocal spell spelllang=nl
setlocal makeprg=msgfmt\ --check\ --statistics\ -o\ /dev/null\ %
setlocal scrolloff=10

inoremap <buffer> ... …
inoremap <buffer> `` ‘
inoremap <buffer> '' ’

noremap <buffer> <leader>uu /\([^\\]\\|^\)""\n\n/b+2<CR>:nohlsearch<CR>
noremap <buffer> <leader>ff /, fuzzy<CR>/msgstr\(\[\d\]\)\? "/e+1<CR>:nohlsearch<CR>
noremap <buffer> <leader>df :FuzzyDel<CR>
noremap <buffer> <leader>cm ?^msgid<CR>V/msgstr<CR>kyjpkdd:s/^msgid/msgstr/<CR>:nohlsearch<CR>
nmap <Return> <leader>uu
nmap <C-Return> <leader>ff
nmap <C-S-Return> <leader>df

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
  " Find previous msgid line
  ?^msgid "?

  " Go one line up and get the line content
  -1
  let line = getline(".")

  " If this a msgctxt line, we go one line further up
  if line =~ '^msgctxt "'
    -1
    let line = getline(".")
  endif

  " If the line only contains fuzzy, remove it altogether
  if line =~ '^#, fuzzy$'
    delete
  " Otherwise only remove the fuzzy marker
  else
    substitute/, fuzzy//
  endif

  " Move to the next message (next paragraph)
  normal }
endfunction
command! FuzzyDel call FuzzyDel()
