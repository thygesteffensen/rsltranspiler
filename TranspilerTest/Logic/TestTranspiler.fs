module TranspilerTest.Logic.TestTranspiler

open NUnit.Framework
open Transpiler
open Transpiler.Helpers
open Transpiler.Ast
open Transpiler.Writer
open TranspilerTest.Common
open TranspilerTest.Compare

[<SetUp>]
let Setup () = ()

type TT =
    | File of System.String
    | Expected of Map<string, TypeDefinition>

let input: obj[] list =
    [ [| "Samples/TypeAbstract.rsl"; Map.empty.Add("T", Abstract) |]
      [| "Samples/TypeConcrete.rsl"; Map.empty.Add("T", Concrete(TName ("Nat", pos 4 17 64 "TypeConcrete.rsl"))) |]
      [| "Samples/TypeUnion.rsl"; Map.empty.Add("T", Union [ ("t1", pos 4 18 62 "TypeUnion.rsl"); ("t2", pos 4 23 67 "TypeUnion.rsl"); ("t3", pos 4 28 72 "TypeUnion.rsl") ]) |] ]

[<TestCaseSource(nameof input)>]
let buildSymbolTableTester source expected =
    let actual =
        match testLexerAndParserFromFile source with
        | Some(_, cls) -> Some(Helpers.buildSymbolTable cls)
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
        
    match (expected, actual) with
    | Some e, Some a ->
        compareScheme(e, a)
        // Assert.AreEqual(expected, actual)
    | None, None -> Assert.Fail("Both none") 
    | None, _ -> Assert.Fail("Expected none")
    | _, None -> Assert.Fail("Actual none")

let input3: obj[] list = [ [| "Samples/ValueGeneric.rsl" |] ]

[<TestCase>]
let test () =
    testLexerAndParserFromFile "Samples/AxiomGeneric_unfolded.rsl" |> ignore
    ()

let postfixSource: obj[] list =
    [ [| Map.empty
             .Add("A", Union([ ("a1", pos 1 2 3 ""); ("a2", pos 1 2 3 ""); ("a3", pos 1 2 3 "") ]))
             .Add("B", Union([ ("b1", pos 1 2 3 ""); ("b2", pos 1 2 3 ""); ("b3", pos 1 2 3 "") ]))
         [ SingleTyping(ISimple("t", pos 1 2 3 ""), TName ("A", pos 1 2 3 "")); SingleTyping(ISimple("y", pos 1 2 3 ""), TName ("B", pos 1 2 3 "")) ]
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
    
    // let prefixes = Transpiler.RuleCollection.TypeRule.buildTypePostfixStrings typeEnv valueEnv typingList
    let prefixes = []

    List.iter (fun (e, a) -> Assert.AreEqual(e, a)) (List.zip expected prefixes)

[<TestCaseSource(nameof input3)>]
let write source =
    let expected = testLexerAndParserFromFile source

    Assert.IsNotNull expected

    write expected.Value
