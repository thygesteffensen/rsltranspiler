module TranspilerTest.Logic.TestAxiomUnfolding

open System
open NUnit.Framework
open Transpiler
open Transpiler.Reader

let input1: obj[] list =
    [ [| "Samples/AxiomMissing.rsl"; "A_t3 is expected to have a accompanying axiom (6:13 AxiomMissing.rsl)" |] ]

[<TestCaseSource(nameof input1)>]
let unfoldSpecificationError source expectedMessage =
    match testLexerAndParserFromFile source with
    | Some(scheme) ->
        let m = Assert.Throws<Exception> (fun () -> Transpiler.transpile scheme |> ignore)
        Assert.AreEqual(expectedMessage, m.Message)
    | None -> Assert.Fail("Expected an exception")
