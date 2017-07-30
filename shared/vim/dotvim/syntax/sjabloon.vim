" Vim syntax file
" Language: Sjabloon
" Maintainer: Tom Sydney Kerckhove
" Latest Revision: 2017-07-30

if exists("b:current_syntax")
  finish
endif

" Keywords
syn keyword syntaxElementKeyword keyword1 keyword2 nextgroup=syntaxElement2

" Matches
syn match syntaxElementMatch 'regexp' contains=syntaxElement1 nextgroup=syntaxElement2 skipwhite

" Regions
syn region syntaxElementRegion start='x' end='y'

