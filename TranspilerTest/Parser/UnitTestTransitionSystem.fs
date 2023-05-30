module TranspilerTest.Parser.TransitionSystem

open NUnit.Framework
open Transpiler.Ast
open TranspilerTest.Common

let f1 = "TransitionSystem.rsl"
let input: obj[] list =
    [ [| "Samples/TransitionSystem.rsl"
         Scheme(
             ("TransitionSystem", pos 1 8 7 f1),
             [ TypeDeclaration([ (("Pos", pos 4 13 64 f1), Union([ ("p1", pos 4 20 71 f1); ("p2", pos 4 25 76 f1) ])) ])
               TransitionSystemDeclaration(
                   (("TS", pos 5 29 108 f1),
                    [ Variable(
                          [ Typing(SingleTyping(ISimple ("v1", pos 7 17 152 f1), TName ("Nat", pos 7 22 157 f1)))
                            Typing(SingleTyping(IGeneric(("v2", pos 8 17 179 f1), [ SingleTyping(ISimple("t", pos 8 22 184 f1), TName ("Pos", pos 8 26 188 f1)) ]), TName ("Nat", pos 8 34 196 f1)))
                            ExplicitValue(ISimple(("v3", pos 9 17 218 f1)), TName ("Nat", pos 9 22 223 f1), ValueLiteral((VInt 3, pos 9 29 230 f1))) ]
                      )
                      InitConstraint(
                          [ Infix(VName(ASimple ("v1", pos 11 17 278 f1)), Equal, ValueLiteral((VInt 1, pos 11 22 283 f1)))
                            Infix(VName(ASimple ("v3", pos 12 17 305 f1)), Equal, ValueLiteral((VInt 3, pos 12 22 310 f1)))
                            Quantified(
                                All,
                                [ SingleTyping(ISimple("t", pos 13 22 337 f1), TName ("Pos", pos 13 26 341 f1)) ],
                                Infix(VName(AGeneric(("v2", pos 13 33 348 f1), [ VName(ASimple ("t", pos 13 36 351 f1)) ])), Equal, ValueLiteral((VInt 2, pos 13 41 356 f1)))
                            ) ]
                      )
                      TransitionRule(
                          Infix(
                              Infix(
                                  Infix(
                                      Infix(ValueLiteral((VBool true, pos 16 18 421 f1)), Equal, ValueLiteral((VBool true, pos 16 25 428 f1))),
                                      Guard,
                                      Infix(
                                          VPName(AGeneric(("v2", pos 17 21 458 f1), [ VName(ASimple ("p1", pos 17 25 462 f1)) ])),
                                          Equal,
                                          Infix(VName(AGeneric(("v2", pos 17 31 468 f1), [ VName(ASimple ("p1", pos 17 34 471 f1)) ])), Plus, ValueLiteral((VInt 1, pos 17 40 477 f1)))
                                      )
                                  ),
                                  Deterministic,
                                  Infix(
                                      Infix(ValueLiteral((VBool true, pos 19 18 519 f1)), Equal, ValueLiteral((VBool false, pos 19 25 526 f1))),
                                      Guard,
                                      Infix(VPName(ASimple ("v1", pos 20 17 553 f1)), Equal, Infix(VName(ASimple ("v3", pos 20 23 559 f1)), Plus, ValueLiteral((VInt 1, pos 20 28 564 f1))))
                                  )
                              ),
                              NonDeterministic,
                              Quantified(
                                  Quantifier.NonDeterministic,
                                  [ SingleTyping(ISimple("t", pos 22 22 610 f1), TName ("Pos", pos 22 26 614 f1)) ],
                                  Infix(
                                      Infix(ValueLiteral((VBool false, pos 22 33 621 f1)), Equal, ValueLiteral((VBool false, pos 22 41 629 f1))),
                                      Guard,
                                      Infix(
                                          VPName(AGeneric(("v2", pos 22 51 639 f1), [ VName(ASimple ("t", pos 22 55 643 f1)) ])),
                                          Equal,
                                          Infix(VName(AGeneric(("v2", pos 22 60 648 f1), [ VName(ASimple ("t", pos 22 63 651 f1)) ])), Plus, ValueLiteral((VInt 1, pos 22 68 656 f1)))
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
