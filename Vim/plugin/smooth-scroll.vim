" Smooth Scroll
"
" Allow smooth scrolling of the window when certain movement commands are
" used. This helps keeping track of the text.
"
" Originally written by Brad Phelan, 2006, http://xtargets.com
" Adapted by by Wouter Bolsterlee, 2009
"
" The global variable g:scroll_factor changes the scroll speed.
"

let g:scroll_factor = 2000
function! SmoothScroll(dir, windiv, factor)
   let wh=winheight(0)
   let i=0
   while i < wh / a:windiv
      let t1=reltime()
      let i = i + 1
      if a:dir=="d"
         normal j
      else
         normal k
      end
      redraw
      while 1
         let t2=reltime(t1,reltime())
         if t2[1] > g:scroll_factor * a:factor
            break
         endif
      endwhile
   endwhile
endfunction


" Functions to enable and disable the keyboard mappings. This will remap or
" unmap <C-U>, <C-D>, <C-F>, and <C-B>.

function! SmoothScrolling()
	map <C-D> :call SmoothScroll("d", 2, 2)<CR>
	map <C-U> :call SmoothScroll("u", 2, 2)<CR>
	map <C-F> :call SmoothScroll("d", 1, 1)<CR>
	map <C-B> :call SmoothScroll("u", 1, 1)<CR>
endfunction

function! NoSmoothScrolling()
	unmap <C-D>
	unmap <C-U>
	unmap <C-F>
	unmap <C-B>
endfunction


" Commands to call the functions, so that one can type
"   :SmoothScrolling<Enter>    to enable smooth scrolling, and
"   :NoSmoothScrolling<Enter>  to disable smooth scrolling, and

command! SmoothScrolling call SmoothScrolling()
command! NoSmoothScrolling call NoSmoothScrolling()
