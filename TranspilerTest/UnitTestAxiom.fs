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
             [ Value([ Typing(SingleTyping(ISimple("A", pos 4 13 57 f1), TName("Text", pos 4 16 60 f1))) ])
               AxiomDeclaration(
                   [ Infix(VName(ASimple("A", pos 6 13 91 f1)), Equal, ValueLiteral((VText "a", pos 6 17 95 f1))) ]
               ) ]
         ) |]
      [| "Samples/" + f2
         Scheme(
             ("AxiomGeneric", pos 1 8 7 f2),
             [ TypeDeclaration(
                   [ (("Pos", pos 4 13 57 f2),
                      Union([ ("t1", pos 4 20 64 f2); ("t2", pos 4 25 69 f2); ("t3", pos 4 30 74 f2) ])) ]
               )
               Value(
                   [ Typing(
                         SingleTyping(
                             IGeneric(
                                 ("A", pos 6 13 103 f2),
                                 [ SingleTyping(ISimple("t", pos 6 17 107 f2), TName("Pos", pos 6 20 110 f2)) ]
                             ),
                             TName("Nat", pos 6 28 118 f2)
                         )
                     ) ]
               )
               AxiomDeclaration(
                   [ Quantified(
                         (All, pos 8 14 150 f2),
                         [ SingleTyping(ISimple("x", pos 8 18 154 f2), TName("Pos", pos 8 22 158 f2)) ],
                         Infix(
                             VName(AGeneric(("A", pos 8 29 165 f2), [ VName(ASimple("x", pos 8 31 167 f2)) ])),
                             Equal,
                             ValueLiteral((VInt 2, pos 8 36 172 f2))
                         )
                     ) ]
               ) ]
         ) |]
      [| "Samples/" + f3
         Scheme(
             ("AxiomGeneric2", pos 1 8 7 f3),
             [ TypeDeclaration(
                   [ (("Pos", pos 4 13 58 f3),
                      Union([ ("t1", pos 4 20 65 f3); ("t2", pos 4 25 70 f3); ("t3", pos 4 30 75 f3) ])) ]
               )
               Value(
                   [ Typing(
                         SingleTyping(
                             IGeneric(
                                 ("A", pos 6 13 104 f3),
                                 [ SingleTyping(ISimple("t1", pos 6 17 108 f3), TName("Pos", pos 6 21 112 f3))
                                   SingleTyping(ISimple("t2", pos 6 26 117 f3), TName("Pos", pos 6 30 121 f3)) ]
                             ),
                             TName("Nat", pos 6 38 129 f3)
                         )
                     ) ]
               )
               AxiomDeclaration(
                   [ Quantified(
                         (All, pos 8 13 160 f3),
                         [ SingleTyping(ISimple("x", pos 8 17 164 f3), TName("Pos", pos 8 21 168 f3)) ],
                         Infix(
                             VName(
                                 AGeneric(
                                     ("A", pos 8 28 175 f3),
                                     [ VName(ASimple("x", pos 8 30 177 f3)); VName(ASimple("x", pos 8 33 180 f3)) ]
                                 )
                             ),
                             Equal,
                             ValueLiteral((VInt 2, pos 8 38 185 f3))
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
