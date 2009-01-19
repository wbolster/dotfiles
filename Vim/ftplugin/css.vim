" Vim configuration for CSS
" Wouter Bolsterlee <uws@xs4all.nl>

setlocal ts=1 sw=1 expandtab

" Sort declaration block alphabetically
noremap <buffer> ,s mxViB:!sort<Cr>`x

" Declaration block
inoreabbrev <buffer> {}  {<CR>}<C-O>O

" Colors
inoreabbrev <buffer> bc: background-color: ;<Left><C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> c: color: ;<Left><C-R>=EatWhitespace()<Cr>

" Fonts and text styles
inoreabbrev <buffer> ff: font-family: ;<Left><C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> fs: font-style: ;<Left><C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> fw: font-weight: ;<Left><C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> fwb font-weight: bold;<C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> fwn font-weight: normal;<C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> ta: text-align: ;<Left><C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> tal text-align: left;<C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> tac text-align: center;<C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> tar text-align: right;<C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> taj text-align: justify;<C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> td: text-decoration: ;<Left><C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> tdn text-decoration: none;<C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> tdu text-decoration: underline;<C-R>=EatWhitespace()<Cr>

" Margin
inoreabbrev <buffer> m: margin: ;<Left><C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> mt: margin-top: ;<Left><C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> mr: margin-right: ;<Left><C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> mb: margin-bottom: ;<Left><C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> ml: margin-left: ;<Left><C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> m0 margin: 0;<C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> mt0 margin-top: 0;<C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> mr0 margin-right: 0;<C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> mb0 margin-bottom: 0;<C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> ml0 margin-left: 0;<C-R>=EatWhitespace()<Cr>

" Padding
inoreabbrev <buffer> p: padding: ;<Left><C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> pt: padding-top: ;<Left><C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> pr: padding-right: ;<Left><C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> pb: padding-bottom: ;<Left><C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> pl: padding-left: ;<Left><C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> p0 padding: 0;<C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> pt0 padding-top: 0;<C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> pr0 padding-right: 0;<C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> pb0 padding-bottom: 0;<C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> pl0 padding-left: 0;<C-R>=EatWhitespace()<Cr>

" Border
inoreabbrev <buffer> b: border: ;<Left><C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> bt: border-top: ;<Left><C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> br: border-right: ;<Left><C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> bb: border-bottom: ;<Left><C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> bl: border-left: ;<Left><C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> b0 border: none;<C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> bt0 border-top: none;<C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> br0 border-right: none;<C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> bb0 border-bottom: none;<C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> bl0 border-left: none;<C-R>=EatWhitespace()<Cr>

" Blocks and floats
inoreabbrev <buffer> db display: block;<C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> d: display: ;<Left><C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> di display: inline;<C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> fl: float: ;<Left><C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> fll float: left;<C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> flr float: right;<C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> h: height: ;<Left><C-R>=EatWhitespace()<Cr>
inoreabbrev <buffer> w: width: ;<Left><C-R>=EatWhitespace()<Cr>
