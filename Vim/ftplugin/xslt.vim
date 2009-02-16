" Vim configuration for XSLT
" Wouter Bolsterlee <uws@xs4all.nl>


" xsl:stylesheet
iabbrev <buffer> xst <xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0"><Cr></xsl:stylesheet><C-O>O

" xsl:template
iabbrev <buffer> xt <xsl:template match=""><Cr></xsl:template><Up><C-O>^<C-O>2f"<C-R>=EatWhitespace()<CR>
iabbrev <buffer> xtn <xsl:template name=""><Cr></xsl:template><Up><C-O>^<C-O>2f"<C-R>=EatWhitespace()<CR>

" xsl:choose
iabbrev <buffer> xc <xsl:choose><Cr></xsl:choose><C-O>F<<C-R>=EatWhitespace()<CR><C-O>O

" xsl:when
iabbrev <buffer> xw <xsl:when test=""><Cr></xsl:when><Up><C-O>^<C-O>2f"<C-R>=EatWhitespace()<CR>

" xsl:otherwise
iabbrev <buffer> xo <xsl:otherwise><Cr></xsl:otherwise><C-O>F<<C-R>=EatWhitespace()<CR><C-O>O

" xsl:if
iabbrev <buffer> xi <xsl:if test=""><Cr></xsl:if><Up><C-O>^<C-O>2f"<C-R>=EatWhitespace()<CR>

" xsl:value-of
iabbrev <buffer> xvo <xsl:value-of select=""/><C-O>F"<C-R>=EatWhitespace()<CR>

" xsl:apply-templates
iabbrev <buffer> xats <xsl:apply-templates select=""/><C-O>F"<C-R>=EatWhitespace()<CR>
iabbrev <buffer> xat <xsl:apply-templates/><C-O>F"<C-R>=EatWhitespace()<CR>

" xsl:call-template
iabbrev <buffer> xct <xsl:call-template name=""/><C-O>F"<C-R>=EatWhitespace()<CR>

" xsl:text
iabbrev <buffer> xtxt <xsl:text></xsl:text><C-O>F<<C-R>=EatWhitespace()<CR>

" xsl:message
iabbrev <buffer> xmsg <xsl:message></xsl:message><C-O>F<<C-R>=EatWhitespace()<CR>

" xsl:for-each
iabbrev <buffer> xfe <xsl:for-each select=""><Cr></xsl:for-each><Up><C-O>^<C-O>2f"<C-R>=EatWhitespace()<CR>

" xsl:attribute
iabbrev <buffer> xa <xsl:attribute name=""><Cr></xsl:attribute><Up><C-O>^<C-O>2f"<C-R>=EatWhitespace()<CR>

" xsl:variable
iabbrev <buffer> xvar <xsl:variable name=""><Cr></xsl:variable><Up><C-O>^<C-O>2f"<C-R>=EatWhitespace()<CR>
