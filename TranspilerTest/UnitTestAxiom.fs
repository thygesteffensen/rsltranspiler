module TranspilerTest.Axiom

open NUnit.Framework
open Transpiler
open TranspilerTest.Common

let input: obj[] list =
    [ [| "Samples/AxiomSimple.rsl"
         Scheme(
             "AxiomSimple",
             [ Value([ Typing(SingleTyping("A", TName "Text")) ])
               AxiomDeclaration([ Equivalence(VName "A", ValueLiteral(VText "a")) ]) ]
         ) |]
      [| "Samples/AxiomGeneric.rsl"
         Scheme(
             "AxiomGeneric",
             [ TypeDeclaration([ ("Pos", Union([ "t1"; "t2"; "t3" ])) ])
               Value([ GenericValue("A", [ SingleTyping("t", TName "Pos") ], TName "Nat") ])
               AxiomDeclaration(
                   [ Quantified(
                         All,
                         [ SingleTyping("x", TName "Pos") ],
                         Equivalence(GenericName("A", [ VName "x" ]), ValueLiteral(VInt 2))
                     ) ]
               ) ]
         ) |]
      [| "Samples/AxiomGeneric2.rsl"
         Scheme(
             "AxiomGeneric",
             [ TypeDeclaration([ ("Pos", Union([ "t1"; "t2"; "t3" ])) ])
               Value(
                   [ GenericValue(
                         "A",
                         [ SingleTyping("t1", TName "Pos"); SingleTyping("t2", TName "Pos") ],
                         TName "Nat"
                     ) ]
               )
               AxiomDeclaration(
                   [ Quantified(
                         All,
                         [ SingleTyping("x", TName "Pos") ],
                         Equivalence(GenericName("A", [ VName "x"; VName "x" ]), ValueLiteral(VInt 2))
                     ) ]
               ) ]
         ) |] ]

[<TestCaseSource(nameof input)>]
let TestValueNat (source: string) expected =
    let actual = testLexerAndParserFromFile source

    match actual with
    | Some t -> Assert.AreEqual(expected, t)
    | None -> Assert.Fail "Should succeed"
