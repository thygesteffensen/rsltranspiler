module TranspilerTest.Logic.TestAxiomUnfolding

open System
open NUnit.Framework
open Transpiler
open TranspilerTest.Common

let input1: obj[] list =
    [ [| "Samples/AxiomMissing.rsl"; "The following unfolded generics are missing: A_t1, A_t3" |] ]

[<TestCaseSource(nameof input1)>]
let unfoldSpecificationError source expectedMessage =
    match testLexerAndParserFromFile source with
    | Some(scheme) ->
        let m = Assert.Throws<Exception> (fun () -> Transpiler.transpile scheme |> ignore)
        Assert.AreEqual(expectedMessage, m.Message)
    | None -> Assert.Fail("Expected an exception")
