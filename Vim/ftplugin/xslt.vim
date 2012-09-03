" Vim configuration for XSLT


setlocal iskeyword+=:  " needed for abbreviations


" Abbreviations for templates

iabbrev <buffer> x:t     <xsl:template match=""><Enter></xsl:template><Up><C-O>^<C-O>2f"<C-R>=EatWhitespace()<CR>
iabbrev <buffer> x:tn    <xsl:template name=""><Enter></xsl:template><Up><C-O>^<C-O>2f"<C-R>=EatWhitespace()<CR>
iabbrev <buffer> x:at    <xsl:apply-templates/><C-O>F"<C-R>=EatWhitespace()<CR>
iabbrev <buffer> x:ats   <xsl:apply-templates select=""/><C-O>F"<C-R>=EatWhitespace()<CR>
iabbrev <buffer> x:ct    <xsl:call-template name=""/><C-O>F"<C-R>=EatWhitespace()<CR>


" Control flow abbreviations

iabbrev <buffer> x:fe    <xsl:for-each select=""><Enter></xsl:for-each><Up><C-O>^<C-O>2f"<C-R>=EatWhitespace()<CR>
iabbrev <buffer> x:ch    <xsl:choose><Enter></xsl:choose><C-O>F<<C-R>=EatWhitespace()<CR><C-O>O
iabbrev <buffer> x:when  <xsl:when test=""><Enter></xsl:when><Up><C-O>^<C-O>2f"<C-R>=EatWhitespace()<CR>
iabbrev <buffer> x:if    <xsl:if test=""><Enter></xsl:if><Up><C-O>^<C-O>2f"<C-R>=EatWhitespace()<CR>
iabbrev <buffer> x:o     <xsl:otherwise><Enter></xsl:otherwise><C-O>F<<C-R>=EatWhitespace()<CR><C-O>O


" Miscellaneous Abbreviations

iabbrev <buffer> x:st    <xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0"><Enter></xsl:stylesheet><C-O>O
iabbrev <buffer> x:vo    <xsl:value-of select=""/><C-O>F"<C-R>=EatWhitespace()<CR>
iabbrev <buffer> x:txt   <xsl:text></xsl:text><C-O>F<<C-R>=EatWhitespace()<CR>
iabbrev <buffer> x:msg   <xsl:message></xsl:message><C-O>F<<C-R>=EatWhitespace()<CR>
iabbrev <buffer> x:attr  <xsl:attribute name=""><Enter></xsl:attribute><Up><C-O>^<C-O>2f"<C-R>=EatWhitespace()<CR>
iabbrev <buffer> x:var   <xsl:variable name=""><Enter></xsl:variable><Up><C-O>^<C-O>2f"<C-R>=EatWhitespace()<CR>
