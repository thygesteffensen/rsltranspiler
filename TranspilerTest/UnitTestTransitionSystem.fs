module TranspilerTest.UnitTestTransitionSystem

open NUnit.Framework
open Transpiler
open TranspilerTest.Common
open TranspilerTest.Compare

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

    Writer.write expected.Value "/home/thyge/Downloads/Expected.rsl"
    Writer.write actual.Value "/home/thyge/Downloads/Actual.rsl"
    match expected, actual with
    | None, None -> ()
    | Some e, Some a -> compareScheme(e, a)
    | _ -> Assert.Fail("Not equal")
