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
    let expected =
        Scheme("ValueNat", [(Value [ ExplicitValue("T", Name "Nat", None) ])])

    let actual = testLexerAndParserFromFile "Samples/ValueNat.rsl"

    match actual with
    | Some t -> Assert.AreEqual(expected, t)
    | None -> Assert.Fail "Should succeed"
    
[<Test>]
let TestValueGeneric () =
    let expected =
        Scheme(
            "ValueGeneric", [
                Type([
                    Union("TrainId", ["t1"; "t2"; "t3"])
                ]);
                Value([
                    GenericValue("position", [
                        Typing("t", Name "TrainId")
                    ], Name "Nat")
                ])
            ])

    let actual = testLexerAndParserFromFile "Samples/ValueGeneric.rsl"

    match actual with
    | Some t -> Assert.AreEqual(expected, t)
    | None -> Assert.Fail "Should succeed"
