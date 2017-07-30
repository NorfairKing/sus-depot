" Vim syntax file
" Language: Sjabloon
" Maintainer: Tom Sydney Kerckhove
" Latest Revision: 2017-07-30

if exists("b:current_syntax")
  finish
endif

syntax match SjabVar /${[^\ }]*}/
syntax match SjabIf   /${if\ [^\ }]*}/
syntax match SjabElse /${else}/
syntax match SjabEndif /${endif}/

highlight SjabVar   ctermfg=blue  guifg=#ff0000
highlight SjabIf    ctermfg=red  guifg=#ff0000
highlight SjabElse  ctermfg=red  guifg=#ff0000
highlight SjabEndif ctermfg=red  guifg=#ff0000

