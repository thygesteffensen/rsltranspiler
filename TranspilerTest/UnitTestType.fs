module TranspilerTest.Type

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
let TestTypeAbstract () =
    let expected = Scheme("TypeAbstract", [TypeDeclaration [("T", Abstract)]])

    let actual = testLexerAndParserFromFile "Samples/TypeAbstract.rsl"
    
    match actual with
    | Some t -> Assert.AreEqual(expected, t)
    | None -> Assert.Fail "Should succeed"

[<Test>]
let TestTypeConcrete () =
    let expected =
        Scheme("TypeConcrete", [ TypeDeclaration [ ("T", Concrete(Name "Nat")) ] ])

    let actual = testLexerAndParserFromFile "Samples/TypeConcrete.rsl"

    match actual with
    | Some t -> Assert.AreEqual(expected, t)
    | None -> Assert.Fail "Should succeed"
    
[<Test>]
let TestTypeUnion () =
    let expected = Scheme("TypeUnion", [TypeDeclaration [("T", Union(["t1"; "t2"; "t3"]))]])

    let actual = testLexerAndParserFromFile "Samples/TypeUnion.rsl"
    
    match actual with
    | Some t -> Assert.AreEqual(expected, t)
    | None -> Assert.Fail "Should succeed"

    
