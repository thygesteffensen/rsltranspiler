module TranspilerTest.Logic.TestTranspiler

open NUnit.Framework
open Transpiler
open Transpiler.Writer
open TranspilerTest.Common

[<SetUp>]
let Setup () = ()

type TT =
    | File of System.String
    | Expected of Map<string, TypeDefinition>

let input: obj[] list =
    [ [| "Samples/TypeAbstract.rsl"; Map.empty.Add("T", Abstract) |]
      [| "Samples/TypeConcrete.rsl"; Map.empty.Add("T", Concrete(Name "Nat")) |]
      [| "Samples/TypeUnion.rsl"; Map.empty.Add("T", Union [ "t1"; "t2"; "t3" ]) |] ]

[<TestCaseSource(nameof input)>]
let buildSymbolTableTester source expected =
    let actual =
        match testLexerAndParserFromFile source with
        | Some(_, cls) -> Some(Transpiler.buildSymbolTable cls)
        | None -> None

    match actual with
    | Some t1 -> Assert.AreEqual(expected, t1)
    | None -> Assert.Fail "Should succeed"

let input2: obj[] list =
    [
        [| "Samples/ValueGeneric.rsl"; "Samples/ValueGeneric_unfolded.rsl" |]
        [| "Samples/ValueGeneric2.rsl"; "Samples/ValueGeneric2_unfolded.rsl" |]
        [| "Samples/AxiomGeneric.rsl"; "Samples/AxiomGeneric_unfolded.rsl" |]
    ]

[<TestCaseSource(nameof input2)>]
let unfoldSpecification source expectedSource =
    let expected = testLexerAndParserFromFile expectedSource
    Assert.IsNotNull expected
    
    let actual =
        match testLexerAndParserFromFile source with
        | Some(scheme) -> Some(Transpiler.transpile scheme) 
        | None -> None
        
    Assert.AreEqual(expected, actual)
    
let input3: obj[] list =
    [
        [| "Samples/ValueGeneric.rsl" |]
    ]

[<TestCase>]
let test () =
    testLexerAndParserFromFile "Samples/AxiomGeneric_unfolded.rsl" |> ignore
    ()

[<TestCaseSource(nameof input3)>]
let write source =
    let expected = testLexerAndParserFromFile source
    
// [<Test>]
// let test () =
    // let expected = testLexerAndParserFromFile "Samples/ValueGeneric.rsl"
    Assert.IsNotNull expected
    
    write expected.Value
    