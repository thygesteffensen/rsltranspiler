module TranspilerTest.Parser.TransitionSystem

open NUnit.Framework
open Transpiler
open TranspilerTest.Common

let input: obj[] list =
    [ [| "Samples/TransitionSystem.rsl"
         Scheme(
             "TransitionSystem",
             [ TypeDeclaration([ ("Pos", Union(["p1"; "p2"])) ])
               TransitionSystemDeclaration(
                   ("TS", [
                   Variable([
                       (Simple "v1", TName "Nat", None)
                       (Generic("v2", [SingleTyping("t", TName "Pos")]), TName "Nat", None)
                       (Simple("v3"), TName "Nat", Some(ValueLiteral (VInt 3)))
                   ])
                   InitConstraint([
                       Equivalence(VName "v1", ValueLiteral (VInt 1))
                       Equivalence(VName "v3", ValueLiteral (VInt 3))
                       Quantified(All, [SingleTyping("t", TName "Pos")], Equivalence(GenericName("v2", [VName "t"]), ValueLiteral (VInt 2)))
                   ])
               ])
               ) ]
         ) |] ]

[<TestCaseSource(nameof input)>]
let TestValueNat (source: string) expected =
    let actual = testLexerAndParserFromFile source

    match actual with
    | Some t -> Assert.AreEqual(expected, t)
    | None -> Assert.Fail "Should succeed"
