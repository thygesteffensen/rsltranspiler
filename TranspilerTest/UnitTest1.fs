module TranspilerTest

open NUnit.Framework
open FSharp.Text.Lexing
open Transpiler

[<SetUp>]
let Setup () = ()

let testLexerAndParserFromFile (fileName: string) =
    use textReader = new System.IO.StreamReader(fileName)
    let lexbuf = LexBuffer<char>.FromTextReader textReader


    Parser.start Lexer.read lexbuf


[<Test>]
let TestSimple () =

    let expected = Scheme("Simple", (Type ["T"]))

    let actual = testLexerAndParserFromFile "Samples/Simple.rsl"
    
    match actual with
    | Some t -> Assert.AreEqual(expected, t)
    | None -> Assert.Fail "Should succeed"

    
