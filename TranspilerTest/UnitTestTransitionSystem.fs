module TranspilerTest.UnitTestTransitionSystem

open NUnit.Framework
open Transpiler
open TranspilerTest.Common
open TranspilerTest.Compare

open Transpiler.Helpers.Helpers

[<SetUp>]
let setup () = ()

let testInput: obj[] list =
    [
        [|"Samples/NamedTransitionRules.rsl"; "Samples/NamedTransitionRules_unfolded.rsl"|]
    ]

[<TestCaseSource(nameof testInput)>]
let test input expectedSource =
    let expected = testLexerAndParserFromFile expectedSource
    Assert.IsNotNull expected

    let actual =
        match testLexerAndParserFromFile input with
        | Some(scheme) -> Some(Transpiler.transpile scheme)
        | None -> None

    match expected, actual with
    | None, None -> ()
    | Some e, Some a -> compareScheme(e, a)
    | _ -> Assert.Fail("Not equal")
