if exists("b:current_syntax")
  finish
endif

syntax keyword SBPLKeywords if then else while do func include define declare let end in
syntax keyword SBPLStdOps print swap dup drop reach int64* ptr* char* float64* int64& ptr& char& float64& __syscall1__ __syscall2__ __syscall3__ __syscall4__ __syscall5__ __syscall6__
syntax match SBPLTypeNames /\<\(int64\|float64\|char\|ptr\)\>\ze\(\s\|$\)/

syntax match  SBPLCommentLine /;.*/

syntax region SBPLStringLit start=/"/ skip=/\\./ end=/"/ contains=SBPLEscapes
syntax region SBPLCharLit   start=/'/ skip=/\\./ end=/'/ contains=SBPLEscapes

syntax match SBPLEscapes display contained /\\[ntrba0"']/

syntax match SBPLNumber /\<-\=\d\+\(\.\d\+\)\?\>/

highlight default link SBPLKeywords Keyword
highlight SBPLStdOps guifg=#dccd79
highlight default link SBPLCommentLine Comment
highlight default link SBPLStringLit String
highlight default link SBPLCharLit Character
highlight default link SBPLEscapes SpecialChar
highlight default link SBPLNumber Number
highlight SBPLTypeNames guifg=#36bcb0

let b:current_syntax = "sbpl"
