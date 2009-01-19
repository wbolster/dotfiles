" Vim configuration for XSLT
" Wouter Bolsterlee <uws@xs4all.nl>


"
" Abbreviations
"

" xsl:stylesheet
iabbrev <buffer> xst <xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0"><Cr></xsl:stylesheet><C-O>O

" xsl:template
iabbrev <buffer> xt <xsl:template match=""><Cr></xsl:template><Up><C-O>^<C-O>2f"<C-R>=EatWhitespace()<CR>

" xsl:value-of
iabbrev <buffer> xvo <xsl:value-of select=""/><C-O>F"<C-R>=EatWhitespace()<CR>

" xsl:apply-templates
iabbrev <buffer> xats <xsl:apply-templates select=""/><C-O>F"<C-R>=EatWhitespace()<CR>
iabbrev <buffer> xat <xsl:apply-templates/><C-O>F"<C-R>=EatWhitespace()<CR>

" xsl:template
iabbrev <buffer> xm <xsl:message></xsl:message><C-O>F<<C-R>=EatWhitespace()<CR>

" xsl:for-each
iabbrev <buffer> xfe <xsl:for-each select=""><Cr></xsl:for-each><Up><C-O>^<C-O>2f"<C-R>=EatWhitespace()<CR>

" xsl:attribute
iabbrev <buffer> xa <xsl:attribute match=""><Cr></xsl:attribute><Up><C-O>^<C-O>2f"<C-R>=EatWhitespace()<CR>
