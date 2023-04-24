module TranspilerTest.Logic.TestTranspiler

open NUnit.Framework
open Transpiler
open TranspilerTest.Common

[<SetUp>]
let Setup () = ()

type TT =
    | File of System.String
    | Expected of Map<string, TypeDefinition>

// A little not strongly typed never killed nobody
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
