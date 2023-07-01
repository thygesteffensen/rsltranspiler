module Transpiler.Reader

open FSharp.Text.Lexing

/// <summary>
/// Reads input from <c>fileName</c>, tokenizing and parsing it using the
/// transpiler from <see cref="Transpiler.Transpiler"/>
/// </summary>
/// <param name="filename">Input file</param>
let testLexerAndParserFromFile (filename: string) =
    let path = System.IO.Path.GetFileName(@$"{filename}")
    
    use textReader = new System.IO.StreamReader(filename)
    let lexbuf = LexBuffer<char>.FromTextReader textReader
    
    Lexer.setFilename path

    Parser.start Lexer.read lexbuf