module TranspilerTest.Common

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

/// <summary>
/// Build <see cref="FSharp.Text.Lexing.Position"/> based on input.
/// </summary>
/// <param name="pos_lnum">Line number</param>
/// <param name="pos_cnum">Column number</param>
/// <param name="pos_bol">Absolute character index</param>
/// <param name="pos_fname">File name</param>
let pos (pos_lnum: int) (pos_cnum: int) (pos_bol: int) (pos_fname: string)  =
    { pos_bol = pos_bol
      pos_cnum = pos_cnum
      pos_orig_lnum = pos_lnum
      pos_fname = pos_fname
      pos_lnum = pos_lnum }