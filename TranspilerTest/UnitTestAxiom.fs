module TranspilerTest.Axiom

open NUnit.Framework
open Transpiler.Ast
open TranspilerTest.Common

let f1 = "AxiomSimple.rsl"
let f2 = "AxiomGeneric.rsl"
let f3 = "AxiomGeneric2.rsl"

let input: obj[] list =
    [ [| "Samples/" + f1
         Scheme(
             ("AxiomSimple", pos 1 8 7 f1),
             [ Value([ Typing(SingleTyping(ISimple("A", pos 4 13 60 f1), TName("Text", pos 4 16 63 f1))) ])
               AxiomDeclaration(
                   [ Infix(VName(ASimple("A", pos 6 13 96 f1)), Equal, ValueLiteral((VText "a", pos 6 17 100 f1))) ]
               ) ]
         ) |]
      [| "Samples/" + f2
         Scheme(
             ("AxiomGeneric", pos 1 8 7 f2),
             [ TypeDeclaration(
                   [ (("Pos", pos 4 13 60 f2),
                      Union([ ("t1", pos 4 20 67 f2); ("t2", pos 4 25 72 f2); ("t3", pos 4 30 77 f2) ])) ]
               )
               Value(
                   [ Typing(
                         SingleTyping(
                             IGeneric(
                                 ("A", pos 6 13 108 f2),
                                 [ SingleTyping(ISimple("t", pos 6 17 112 f2), TName("Pos", pos 6 20 115 f2)) ]
                             ),
                             TName("Nat", pos 6 28 123 f2)
                         )
                     ) ]
               )
               AxiomDeclaration(
                   [ Quantified(
                         (All, pos 8 14 157 f2),
                         [ SingleTyping(ISimple("x", pos 8 18 161 f2), TName("Pos", pos 8 22 165 f2)) ],
                         Infix(
                             VName(AGeneric(("A", pos 8 29 172 f2), [ VName(ASimple("x", pos 8 31 174 f2)) ])),
                             Equal,
                             ValueLiteral((VInt 2, pos 8 36 179 f2))
                         )
                     ) ]
               ) ]
         ) |]
      [| "Samples/" + f3
         Scheme(
             ("AxiomGeneric2", pos 1 8 7 f3),
             [ TypeDeclaration(
                   [ (("Pos", pos 4 13 61 f3),
                      Union([ ("t1", pos 4 20 68 f3); ("t2", pos 4 25 73 f3); ("t3", pos 4 30 78 f3) ])) ]
               )
               Value(
                   [ Typing(
                         SingleTyping(
                             IGeneric(
                                 ("A", pos 6 13 109 f3),
                                 [ SingleTyping(ISimple("t1", pos 6 17 113 f3), TName("Pos", pos 6 21 117 f3))
                                   SingleTyping(ISimple("t2", pos 6 26 122 f3), TName("Pos", pos 6 30 126 f3)) ]
                             ),
                             TName("Nat", pos 6 38 134 f3)
                         )
                     ) ]
               )
               AxiomDeclaration(
                   [ Quantified(
                         (All, pos 8 13 167 f3),
                         [ SingleTyping(ISimple("x", pos 8 17 171 f3), TName("Pos", pos 8 21 175 f3)) ],
                         Infix(
                             VName(
                                 AGeneric(
                                     ("A", pos 8 28 182 f3),
                                     [ VName(ASimple("x", pos 8 30 184 f3)); VName(ASimple("x", pos 8 33 187 f3)) ]
                                 )
                             ),
                             Equal,
                             ValueLiteral((VInt 2, pos 8 38 192 f3))
                         )
                     ) ]
               ) ]
         ) |] ]

[<TestCaseSource(nameof input)>]
let TestValueNat (source: string) expected =
    let actual = testLexerAndParserFromFile source

    match actual with
    | Some t -> Assert.AreEqual(expected, t)
    | None -> Assert.Fail "Should succeed"
