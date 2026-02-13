" Based on the syntax highlighting for the c3 programming language
" Original: https://github.com/c3lang/editor-plugins/blob/main/vim/syntax/c3.vim

if exists("b:current_syntax")
  finish
endif

syn match bufoIdentifier display "_*[a-z][a-zA-Z0-9_]*"
syn match bufoUserType display "_*[A-Z][a-zA-Z0-9_]\+"
syn match bufoFunction display "\zs\(\w*\)*\s*\ze("
syn match bufoBuiltinFn display "@_*[a-zA-Z0-9_]*"
syn match bufoNumber display "\v<[0-9](_*[0-9])*([iu](8|16|32|64)|([Uu][Ll]?|[Ll]))?>"

syn match bufoOperator display "\v(\<\<|\>\>|[<>=!+*/%&~^|-])\=?"
syn match bufoDelimiter display "\v[;,:\{\}\(\)\[\].?]"

syn match bufoString display "\v\"(\\.|[^\\\"])*\""
syn match bufoString display "\v`(``|.)*`"

syn region bufoComment display start="\v/\*"   end="\v\*/" contains=bufoComment,bufoTodo
syn region bufoComment display start="\v//"    end="\v$"   contains=bufoTodo

syn keyword bufoTodo TODO FIXME REVIEW contained

syn keyword bufoKeyword
	\ if else match
	\ return
	\ struct func union module
	\ let
	\ comptime asm
	\ import
	\ as
	\ sizeof
	\ type_info

syn keyword bufoRepeat
      \ while
      \ for
      \ continue break

syn keyword bufoBuiltinType Any bool char
syn keyword bufoBuiltinType i8 i16 i32 i64
syn keyword bufoBuiltinType u8 u16 u32 u64 usize
syn keyword bufoBuiltinType f32 f64
syn keyword bufoBuiltinIdent true false this null blank

hi def link bufoBuiltinIdent Include
hi def link bufoBuiltinType  Type
hi def link bufoBuiltinFn    Title

hi def link bufoNumber       Number
hi def link bufoIdentifier   Identifier
hi def link bufoFunction     Function
hi def link bufoUserType     Type
hi def link bufoKeyword      Keyword
hi def link bufoRepeat       Repeat
hi def link bufoString       String
hi def link bufoOperator     Operator
hi def link bufoDelimiter    Delimiter
hi def link bufoComment      Comment
hi def link bufoTodo         Todo

let b:current_syntax = "bufo"

