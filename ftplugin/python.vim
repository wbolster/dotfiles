" pep8-text-width.vim - Set textwidth as per PEP8 style guidelines.
" Maintainer:   Jim Fitzpatrick
" Version:      0.0.1

if exists("b:did_ftplugin")
    finish
endif

if !exists('g:pep8_text_width')
    " Code textwidth.
    let g:pep8_text_width = 79
endif

if !exists('g:pep8_comment_text_width')
    " Comment textwidth.
    let g:pep8_comment_text_width = 72
endif

augroup pep8textwidth
  autocmd! CursorMoved,CursorMovedI <buffer> :exe 'setlocal textwidth='.s:GetCurrentTextWidth()
augroup END

" Return appropriate textwidth for cursor position (leverages syntax engine).
function! s:GetCurrentTextWidth()
    let curr_syntax = synIDattr(synIDtrans(synID(line("."), col("."), 0)), "name")
    let prev_syntax = synIDattr(synIDtrans(synID(line("."), col(".")-1, 0)), "name")
    if curr_syntax =~ 'Comment\|Constant' || prev_syntax =~ 'Comment\|Constant'
        return g:pep8_comment_text_width
    endif

    return g:pep8_text_width
endfunction
