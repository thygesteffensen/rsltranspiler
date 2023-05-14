module TranspilerTest.UnitTestTransitionSystem

open NUnit.Framework
open Transpiler
open TranspilerTest.Common

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

    Assert.AreEqual(expected, actual)
