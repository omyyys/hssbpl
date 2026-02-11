if exists("b:current_syntax")
  finish
endif

syntax keyword SBPLKeywords if then else while do func include define declare let end in
syntax match SBPLStdOps /\v(^|[^A-Za-z0-9_])\zs(__syscall[1-6]__|print|swap|dup|drop|reach)\ze([^A-Za-z0-9_]|$)/
syntax match SBPLTypeNames /\<\%(int64\|float64\|char\|ptr\|string\|bool\|any\)\%(@\%(int64\|float64\|char\|ptr\|string\|bool\|any\)\)*\ze\(\s\|$\)/

syntax match SBPLCommentLine /;.*/

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
