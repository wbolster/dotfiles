function! <SID>BufCloseNoWinClose()
	let l:currentBufNum = bufnr("%")
	let l:alternateBufNum = bufnr("#")

	if buflisted(l:alternateBufNum)
		buffer #
	else
		bnext
	endif

	if bufnr("%") == l:currentBufNum
		new
	endif

	if buflisted(l:currentBufNum)
		execute("bdelete ".l:currentBufNum)
	endif
endfunction 

command! Bc call <SID>BufCloseNoWinClose()
command! Bclose call <SID>BufCloseNoWinClose()
cabbrev bc Bc
