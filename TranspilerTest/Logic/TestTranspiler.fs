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

let emptyStringList: string list = []

let input: obj[] list =
    [ [| "Samples/TypeAbstract.rsl"
         Map.empty.Add("T", (Abstract, emptyStringList)) |]
      [| "Samples/TypeConcrete.rsl"
         Map.empty.Add("T", (Concrete(TName("Nat", pos 4 17 61 "TypeConcrete.rsl")), emptyStringList)) |]
      [| "Samples/TypeUnion.rsl"
         Map.empty.Add(
             "T",
             (Union
                 [ ("t1", pos 4 18 59 "TypeUnion.rsl")
                   ("t2", pos 4 23 64 "TypeUnion.rsl")
                   ("t3", pos 4 28 69 "TypeUnion.rsl") ],
              [ "_t1"; "_t2"; "_t3" ])
         ) |]
      [| "Samples/TypeSubType.rsl"
         Map.empty.Add(
             "ArrayIndex",
             (Concrete(
                 Sub(
                     [ SingleTyping(
                           ISimple("i", pos 4 29 72 "TypeSubType.rsl"),
                           TName("Int", pos 4 33 76 "TypeSubType.rsl")
                       ) ],
                     Infix(
                         Infix(
                             VName(ASimple("i", pos 4 40 83 "TypeSubType.rsl")),
                             GreaterThanOrEqual,
                             ValueLiteral(VInt 0, pos 4 45 88 "TypeSubType.rsl")
                         ),
                         LogicalAnd,
                         Infix(
                             VName(ASimple("i", pos 4 50 93 "TypeSubType.rsl")),
                             LessThan,
                             ValueLiteral(VInt 5, pos 4 54 97 "TypeSubType.rsl")
                         )
                     )
                 )
              ),
              [ "_0"; "_1"; "_2"; "_3"; "_4" ])
         ) |] ]

[<TestCaseSource(nameof input)>]
let buildSymbolTableTester source expected =
    let actual =
        match testLexerAndParserFromFile source with
        | Some(_, cls) -> Some(Helpers.buildSymbolTable cls)
        | None -> None


    match actual with
    | Some t1 -> Assert.That(t1, Is.EquivalentTo(expected))
    | None -> Assert.Fail "Should succeed"

let input2: obj[] list =
    [ [| "Samples/ValueGeneric.rsl"; "Samples/ValueGeneric_unfolded.rsl" |]
      [| "Samples/ValueGeneric2.rsl"; "Samples/ValueGeneric2_unfolded.rsl" |]
      [| "Samples/ValueGeneric3.rsl"; "Samples/ValueGeneric3_unfolded.rsl" |]
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
    | Some e, Some a -> compareScheme (e, a)
    // Assert.AreEqual(expected, actual)
    | None, None -> Assert.Fail("Both none")
    | None, _ -> Assert.Fail("Expected none")
    | _, None -> Assert.Fail("Actual none")


[<TestCase>]
let test () =
    testLexerAndParserFromFile "Samples/AxiomGeneric_unfolded.rsl" |> ignore
    ()

let input3: obj[] list = [ [| "Samples/ValueGeneric.rsl" |] ]

[<TestCaseSource(nameof input3)>]
let write source =
    let expected = testLexerAndParserFromFile source

    Assert.IsNotNull expected

    write expected.Value "/home/thyge/Downloads/temp.txt"
