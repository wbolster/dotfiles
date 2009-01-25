" Vim font size plugin
" Wouter Bolsterlee <uws@xs4all.nl>


" Minimum and maximum font size
let s:minfontsize = 6
let s:maxfontsize = 24


" Function to adjust the font size with a given amount

function! AdjustFontSize(amount)

	if !has("gui_running")
		echoerr "Font size adjustment only works in GVim."
		return
	endif

	if has("gui_gtk2")
		let pattern = '^\(.* \)\([1-9][0-9]*\.\?[0-9]\?\)$'
		let fontname = substitute(&guifont, pattern, '\1', '')
		let cursize = substitute(&guifont, pattern, '\2', '')
		let newsize = cursize + a:amount
		if (newsize >= s:minfontsize) && (newsize <= s:maxfontsize)
			let &guifont = fontname . newsize
		endif
	elseif has("gui_win32")
		let pattern = '^\v(.*:h)(.*)(:.*)$'
		let prefix = substitute(&guifont, pattern, '\1', '')
		let suffix = substitute(&guifont, pattern, '\3', '')
		let cursize = substitute(&guifont, pattern, '\2', '')
		let newsize = cursize + a:amount
		if (newsize >= s:minfontsize) && (newsize <= s:maxfontsize)
			let &guifont = prefix . newsize . suffix
		endif
	else
		echoerr "Font size adjustment only works for GVim when using GTK2 or win32."
	endif
endfunction


" Commands to increase or decrease the font size

command! SmallerFont call AdjustFontSize(-1)
command! LargerFont  call AdjustFontSize(1)
