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
      [| "Samples/TypeConcrete.rsl"; Map.empty.Add("T", Concrete(TName "Nat")) |]
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
    [ [| "Samples/ValueGeneric.rsl"; "Samples/ValueGeneric_unfolded.rsl" |]
      [| "Samples/ValueGeneric2.rsl"; "Samples/ValueGeneric2_unfolded.rsl" |]
      [| "Samples/AxiomGeneric.rsl"; "Samples/AxiomGeneric_unfolded.rsl" |] ]

[<TestCaseSource(nameof input2)>]
let unfoldSpecification source expectedSource =
    let expected = testLexerAndParserFromFile expectedSource
    Assert.IsNotNull expected

    let actual =
        match testLexerAndParserFromFile source with
        | Some(scheme) -> Some(Transpiler.transpile scheme)
        | None -> None

    Assert.AreEqual(expected, actual)

let input3: obj[] list = [ [| "Samples/ValueGeneric.rsl" |] ]

[<TestCase>]
let test () =
    testLexerAndParserFromFile "Samples/AxiomGeneric_unfolded.rsl" |> ignore
    ()

let postfixSource: obj[] list =
    [ [| Map.empty
             .Add("A", Union([ "a1"; "a2"; "a3" ]))
             .Add("B", Union([ "b1"; "b2"; "b3" ]))
         [ SingleTyping("t", TName "A"); SingleTyping("y", TName "B") ]
         [ "_a1_b1"
           "_a2_b1"
           "_a3_b1"
           "_a1_b2"
           "_a2_b2"
           "_a3_b2"
           "_a1_b3"
           "_a2_b3"
           "_a3_b3" ] |] ]

[<TestCaseSource(nameof postfixSource)>]
let rec buildTypePostfixStringsTest typeEnv typingList expected =
    let valueEnv = Map.empty
    
    let prefixes = Transpiler.buildTypePostfixStrings typeEnv valueEnv typingList

    List.iter (fun (e, a) -> Assert.AreEqual(e, a)) (List.zip expected prefixes)

[<TestCaseSource(nameof input3)>]
let write source =
    let expected = testLexerAndParserFromFile source

    // [<Test>]
    // let test () =
    // let expected = testLexerAndParserFromFile "Samples/ValueGeneric.rsl"
    Assert.IsNotNull expected

    write expected.Value
