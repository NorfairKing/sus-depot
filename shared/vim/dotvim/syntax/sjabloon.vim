" Vim syntax file
" Language: Sjabloon
" Maintainer: Tom Sydney Kerckhove
" Latest Revision: 2017-07-30

if exists("b:current_syntax")
  finish
endif

syntax match FooKey   /^[^=]\+/
syntax match FooValue /[^=]\+$/

highlight FooKey   ctermfg=cyan guifg=#00ffff
highlight FooValue ctermfg=red  guifg=#ff0000

