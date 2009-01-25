" Vim syntax file
" Language:	TWiki
" Maintainer:	Simon Clift <ssclift@math.uwaterlooNOSPAMEH.ca>
" Last Change:	2002 Nov 12
" Remark:	First version.
"
" This files was updated by Wouter Bolsterlee <uws@xs4all.nl>, 2007

" Setup
if version >= 600
  if exists("b:current_syntax")
    finish
  endif
else
  syntax clear
endif


" Wiki variables and the like are case sensitive

syn case match


" Wiki Names

syntax match twikiPageName "\([A-Z][a-z]\+\.\)\?\([A-Z][A-Z0-9]*[a-z0-9]\+\)\{2,}"
syntax region twikiPageLink start=/\[\[/ end=/\]\]/


" Base constructs

syntax match twikiBulletPoint "^\(   \)\+\*"
syntax match twikiListNumber  "^\(   \)\d\+"
syntax match twikiDefList     "^\(   \)[^\.]\+: \+"
syntax match twikiSeparator   "^---\+"


" Text highlighting

syntax region twikiFixFormat   oneline start=/=[^=]/ end=/=/
syntax region twikiBFixFormat  oneline start=/==/ end=/==/
syntax region twikiBoldFormat  oneline start=/\*/ end=/\*/
syntax region twikiItalFormat  oneline start=/_[^_]/ end=/[^_]_/
syntax region twikiBItalFormat oneline start=/__/ end=/__/


" Titles

syntax region twikiTitle1 start=/^---+\(!!\)\? / end=/$/
syntax region twikiTitle2 start=/^---++\(!!\)\? / end=/$/
syntax region twikiTitle3 start=/^---+++\(!!\)\? / end=/$/
syntax region twikiTitle4 start=/^---++++\(!!\)\? / end=/$/
syntax region twikiTitle5 start=/^---+++++\(!!\)\? / end=/$/
syntax region twikiTitle6 start=/^---++++++\(!!\)\? / end=/$/


" TWiki variables that are built-in or known in the standard

setlocal iskeyword+=%

syntax keyword twikiVar %ALLOWTOPICCHANGE%
syntax keyword twikiVar %ALLOWTOPICRENAME%
syntax keyword twikiVar %ALLOWTOPICVIEW%
syntax keyword twikiVar %ALLOWWEBCHANGE%
syntax keyword twikiVar %ALLOWWEBRENAME%
syntax keyword twikiVar %ATTACHLINKBOX%
syntax keyword twikiVar %ATTACHURL%
syntax keyword twikiVar %ATTACHURLPATH%
syntax keyword twikiVar %BASETOPIC%
syntax keyword twikiVar %BASEWEB%
syntax keyword twikiVar %DENYTOPICCHANGE%
syntax keyword twikiVar %DENYTOPICRENAME%
syntax keyword twikiVar %DENYWEBCHANGE%
syntax keyword twikiVar %DENYWEBRENAME%
syntax keyword twikiVar %DONTNOTIFYCHECKBOX%
syntax keyword twikiVar %EDITBOXHEIGHT%
syntax keyword twikiVar %EDITBOXWIDTH%
syntax keyword twikiVar %FINALPREFERENCES%
syntax keyword twikiVar %GMTIME%
syntax keyword twikiVar %HOMETOPIC%
syntax keyword twikiVar %HTTP_EQUIV_ON_EDIT%
syntax keyword twikiVar %HTTP_EQUIV_ON_PREVIEW%
syntax keyword twikiVar %HTTP_EQUIV_ON_VIEW%
syntax keyword twikiVar %HTTP_HOST%
syntax keyword twikiVar %INCLUDINGTOPIC%
syntax keyword twikiVar %INCLUDINGWEB%
syntax keyword twikiVar %MAINWEB%
syntax keyword twikiVar %NEWTOPICBGCOLOR%
syntax keyword twikiVar %NEWTOPICFONTCOLOR%
syntax keyword twikiVar %NOSEARCHALL%
syntax keyword twikiVar %NOTIFYTOPIC%
syntax keyword twikiVar %PUBURL%
syntax keyword twikiVar %PUBURLPATH%
syntax keyword twikiVar %RELEASEEDITLOCKCHECKBOX%
syntax keyword twikiVar %REMOTE_ADDR%
syntax keyword twikiVar %REMOTE_PORT%
syntax keyword twikiVar %REMOTE_USER%
syntax keyword twikiVar %SCRIPTSUFFIX%
syntax keyword twikiVar %SCRIPTURL%
syntax keyword twikiVar %SCRIPTURLPATH%
syntax keyword twikiVar %SERVERTIME%
syntax keyword twikiVar %SPACEDTOPIC%
syntax keyword twikiVar %STARTINCLUDE%
syntax keyword twikiVar %STATISTICSTOPIC%
syntax keyword twikiVar %STOPINCLUDE%
syntax keyword twikiVar %TOC%
syntax keyword twikiVar %TOPIC%
syntax keyword twikiVar %TWIKIWEB%
syntax keyword twikiVar %USERNAME%
syntax keyword twikiVar %WEB%
syntax keyword twikiVar %WEBBGCOLOR%
syntax keyword twikiVar %WEBCOPYRIGHT%
syntax keyword twikiVar %WEBPREFSTOPIC%
syntax keyword twikiVar %WEBTOPICLIST%
syntax keyword twikiVar %WIKIHOMEURL%
syntax keyword twikiVar %WIKINAME%
syntax keyword twikiVar %WIKIPREFSTOPIC%
syntax keyword twikiVar %WIKITOOLNAME%
syntax keyword twikiVar %WIKIUSERNAME%
syntax keyword twikiVar %WIKIUSERSTOPIC%
syntax keyword twikiVar %WIKIVERSION%
syntax keyword twikiVar %WIKIWEBLIST%
syntax keyword twikiVar %WIKIWEBMASTER%

syntax region twikiComplexVar start=/%GMTIME{/ end=/}%/
syntax region twikiComplexVar start=/%GMTIME{/ end=/}%/
syntax region twikiComplexVar start=/%INCLUDE{/ end=/}%/
syntax region twikiComplexVar start=/%METASEARCH{/ end=/}%/
syntax region twikiComplexVar start=/%SEARCH{/ end=/}%/
syntax region twikiComplexVar start=/%SERVERTIME{/ end=/}%/
syntax region twikiComplexVar start=/%TOC{/ end=/}%/
syntax region twikiComplexVar start=/%TOPICLIST{/ end=/}%/
syntax region twikiComplexVar start=/%URLPARAM{/ end=/}%/
syntax region twikiComplexVar start=/%VAR{/ end=/}%/
syntax region twikiComplexVar start=/%WEBLIST{/ end=/}%/

highlight link twikiComplexVar twikiVar


" Define the default highlighting
"
if version >= 508 || !exists("did_inittab_syntax_inits")
  if version < 508
    let did_inittab_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink twikiBulletPoint  Type
  HiLink twikiListNumber   Type
  HiLink twikiDefList      Type
  HiLink twikiSeparator    Type

  HiLink twikiFixFormat    String
  HiLink twikiBFixFormat   String
  HiLink twikiBoldFormat   String
  HiLink twikiItalFormat   String
  HiLink twikiBItalFormat  String

  HiLink twikiTitle1       Title
  HiLink twikiTitle2       Title
  HiLink twikiTitle3       Title
  HiLink twikiTitle4       Title
  HiLink twikiTitle5       Title
  HiLink twikiTitle6       Title

  HiLink twikiVar          Macro

  HiLink twikiPageName     Identifier
  HiLink twikiPageLink     Identifier

  delcommand HiLink
endif

let b:current_syntax = "twiki"
