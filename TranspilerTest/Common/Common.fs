module TranspilerTest.Common

open FSharp.Text.Lexing

/// <summary>
/// Reads input from <c>fileName</c>, tokenizing and parsing it using the
/// transpiler from <see cref="Transpiler.Transpiler"/>
/// </summary>
/// <param name="fileName">Input file</param>
let testLexerAndParserFromFile (fileName: string) =
    use textReader = new System.IO.StreamReader(fileName)
    let lexbuf = LexBuffer<char>.FromTextReader textReader

    Parser.start Lexer.read lexbuf
