module TranspilerTest.Parser.TransitionSystem

open NUnit.Framework
open Transpiler.Ast
open TranspilerTest.Common

let f1 = "TransitionSystem.rsl"

let input: obj[] list =
    [ [| "Samples/TransitionSystem.rsl"
         Scheme(
             ("TransitionSystem", pos 1 8 7 f1),
             [ TypeDeclaration([ (("Pos", pos 4 13 61 f1), Union([ ("p1", pos 4 20 68 f1); ("p2", pos 4 25 73 f1) ])) ])
               TransitionSystemDeclaration(
                   (("TS", pos 5 29 104 f1),
                    [ Variable(
                          [ Typing(SingleTyping(ISimple("v1", pos 7 17 146 f1), TName("Nat", pos 7 22 151 f1)))
                            Typing(
                                SingleTyping(
                                    IGeneric(
                                        ("v2", pos 8 17 172 f1),
                                        [ SingleTyping(ISimple("t", pos 8 22 177 f1), TName("Pos", pos 8 26 181 f1)) ]
                                    ),
                                    TName("Nat", pos 8 34 189 f1)
                                )
                            )
                            ExplicitValue(
                                ISimple(("v3", pos 9 17 210 f1)),
                                TName("Nat", pos 9 22 215 f1),
                                ValueLiteral((VInt 3, pos 9 29 222 f1))
                            ) ]
                      )
                      InitConstraint(
                          Infix(
                              Infix(
                                  VName(ASimple("v1", pos 11 17 268 f1)),
                                  Equal,
                                  ValueLiteral((VInt 1, pos 11 22 273 f1))
                              ),
                              LogicalAnd,
                              Infix(
                                  Infix(
                                      VName(ASimple("v3", pos 12 17 294 f1)),
                                      Equal,
                                      ValueLiteral((VInt 3, pos 12 22 299 f1))
                                  ),
                                  LogicalAnd,
                                  Quantified(
                                      (All, pos 13 18 321 f1),
                                      [ SingleTyping(ISimple("t", pos 13 22 325 f1), TName("Pos", pos 13 26 329 f1)) ],
                                      Infix(
                                          VName(
                                              AGeneric(
                                                  ("v2", pos 13 33 336 f1),
                                                  [ VName(ASimple("t", pos 13 36 339 f1)) ]
                                              )
                                          ),
                                          Equal,
                                          ValueLiteral((VInt 2, pos 13 41 344 f1))
                                      )
                                  )
                              )
                          )
                      )
                      TransitionRule(
                          Infix(
                              Infix(
                                  Infix(
                                      ValueLiteral((VBool true, pos 16 18 406 f1)),
                                      Equal,
                                      ValueLiteral((VBool true, pos 16 25 413 f1))
                                  ),
                                  Guard,
                                  VeList[Infix(
                                             VPName(
                                                 AGeneric(
                                                     ("v2", pos 17 21 442 f1),
                                                     [ VName(ASimple("p1", pos 17 25 446 f1)) ]
                                                 )
                                             ),
                                             Equal,
                                             Infix(
                                                 VName(
                                                     AGeneric(
                                                         ("v2", pos 17 31 452 f1),
                                                         [ VName(ASimple("p1", pos 17 34 455 f1)) ]
                                                     )
                                                 ),
                                                 Plus,
                                                 ValueLiteral((VInt 1, pos 17 40 461 f1))
                                             )
                                         )]
                              ),
                              Deterministic,
                              Infix(
                                  Infix(
                                      Infix(
                                          ValueLiteral((VBool true, pos 19 18 501 f1)),
                                          Equal,
                                          ValueLiteral((VBool false, pos 19 25 508 f1))
                                      ),
                                      Guard,
                                      VeList[Infix(
                                                 VPName(ASimple("v1", pos 20 17 534 f1)),
                                                 Equal,
                                                 Infix(
                                                     VName(ASimple("v3", pos 20 23 540 f1)),
                                                     Plus,
                                                     ValueLiteral((VInt 1, pos 20 28 545 f1))
                                                 )
                                             )]
                                  ),
                                  NonDeterministic,
                                  Quantified(
                                      (Quantifier.NonDeterministic, pos 22 18 585 f1),
                                      [ SingleTyping(ISimple("t", pos 22 22 589 f1), TName("Pos", pos 22 26 593 f1)) ],
                                      Infix(
                                          Infix(
                                              ValueLiteral((VBool false, pos 22 33 600 f1)),
                                              Equal,
                                              ValueLiteral((VBool false, pos 22 41 608 f1))
                                          ),
                                          Guard,
                                          VeList[Infix(
                                                     VPName(
                                                         AGeneric(
                                                             ("v2", pos 22 51 618 f1),
                                                             [ VName(ASimple("t", pos 22 55 622 f1)) ]
                                                         )
                                                     ),
                                                     Equal,
                                                     Infix(
                                                         VName(
                                                             AGeneric(
                                                                 ("v2", pos 22 60 627 f1),
                                                                 [ VName(ASimple("t", pos 22 63 630 f1)) ]
                                                             )
                                                         ),
                                                         Plus,
                                                         ValueLiteral((VInt 1, pos 22 68 635 f1))
                                                     )
                                                 )]
                                      )
                                  )
                              )
                          ),
                          []
                      ) ])
               ) ]
         ) |] ]

[<TestCaseSource(nameof input)>]
let TestValueNat (source: string) expected =
    let actual = testLexerAndParserFromFile source

    match actual with
    | Some t -> Assert.AreEqual(expected, t)
    | None -> Assert.Fail "Should succeed"
