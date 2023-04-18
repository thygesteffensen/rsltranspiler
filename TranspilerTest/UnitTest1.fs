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
let TestTypeAbstract () =
    let expected = Scheme("TypeAbstract", (Type [Abstract("T")]))

    let actual = testLexerAndParserFromFile "Samples/TypeAbstract.rsl"
    
    match actual with
    | Some t -> Assert.AreEqual(expected, t)
    | None -> Assert.Fail "Should succeed"

[<Test>]
let TestTypeConcrete () =
    let expected = Scheme("TypeConcrete", (Type [Concrete("T", Name("Nat"))]))

    let actual = testLexerAndParserFromFile "Samples/TypeConcrete.rsl"
    
    match actual with
    | Some t -> Assert.AreEqual(expected, t)
    | None -> Assert.Fail "Should succeed"
    
[<Test>]
let TestTypeUnion () =
    let expected = Scheme("TypeUnion", (Type [Union("T", ["t1"; "t2"; "t3"])]))

    let actual = testLexerAndParserFromFile "Samples/TypeUnion.rsl"
    
    match actual with
    | Some t -> Assert.AreEqual(expected, t)
    | None -> Assert.Fail "Should succeed"

    
