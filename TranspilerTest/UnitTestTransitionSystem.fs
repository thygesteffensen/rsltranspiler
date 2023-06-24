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

    Writer.write expected.Value "/home/thyge/Downloads/Expected.rsl"
    Writer.write actual.Value "/home/thyge/Downloads/Actual.rsl"
    
    ///
    
    (*let specification, cls as e1 = (testLexerAndParserFromFile "/home/thyge/Downloads/Expected.rsl").Value
    let a1' = convertToIntermediate
                cls
                { Type = None
                  Value = None
                  Axiom = None
                  TransitionSystem = None }
    let a1 = (($"{fst specification}", snd specification), convertToAst a1')
    
    compareScheme(e1, a1)*)
    
    (*match expected, actual with
    | None, None -> ()
    | Some e, Some a -> compareScheme(e, a)
    | _ -> Assert.Fail("Not equal")*)
