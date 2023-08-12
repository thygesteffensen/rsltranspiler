#r "nuget: FsLexYacc.Runtime"
open FSharp.Text.Lexing

#load "Ast.fs"
#load "Parser.fs"
#load "Lexer.fs"

let lexbuf = LexBuffer<char>.FromString "+ * 5 6 7"

Parser.start Lexer.token lexbuf
