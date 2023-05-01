module TranspilerTest.Axiom

open NUnit.Framework
open Transpiler
open TranspilerTest.Common

let input: obj[] list =
    [ [| "Samples/AxiomSimple.rsl"
         Scheme(
             "AxiomSimple",
             [ Value([ Typing(SingleTyping("A", Name "Text")) ])
               AxiomDeclaration([ Axiom(Id "A", ValueLiteral(VText "a")) ]) ]
         ) |]
      [| "Samples/AxiomGeneric.rsl"
         Scheme(
             "AxiomGeneric",
             [ TypeDeclaration([ ("Pos", Union([ "t1"; "t2"; "t3" ])) ])
               Value([ GenericValue("A", [ SingleTyping("t", Name "Pos") ], Name "Nat") ])
               AxiomDeclaration([ Forall([ SingleTyping("t", Name "Nat") ], Axiom(Id "A", ValueLiteral(VInt 2))) ]) ]
         ) |] ]

[<TestCaseSource(nameof input)>]
let TestValueNat (source: string) expected =
    let actual = testLexerAndParserFromFile source

    match actual with
    | Some t -> Assert.AreEqual(expected, t)
    | None -> Assert.Fail "Should succeed"
