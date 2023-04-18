module TranspilerTest.Value

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
let TestValueNat () =
    let expected = Scheme("ValueNat", (Value "T"))

    let actual = testLexerAndParserFromFile "Samples/TypeAbstract.rsl"
    
    match actual with
    | Some t -> Assert.AreEqual(expected, t)
    | None -> Assert.Fail "Should succeed"

    
