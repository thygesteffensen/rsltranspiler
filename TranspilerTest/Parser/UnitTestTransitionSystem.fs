module TranspilerTest.Parser.TransitionSystem

open NUnit.Framework
open Transpiler.Ast
open Transpiler.Reader
open TranspilerTest.Common

let f1 = "TransitionSystem.rsl"
let f2 = "SimpleRail.rsl"

let input: obj[] list =
    [ [| $"Samples/{f1}"
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
         ) |]
      [| $"Samples/{f2}"
         (("SimpleRail", pos 1 8 7 f2),
          [ TypeDeclaration
                [ (("TrainId", pos 4 13 56 f2), Union [ ("t1", pos 4 24 67 f2); ("t2", pos 4 29 72 f2) ])
                  (("SegmentId", pos 5 13 88 f2),
                   Concrete(
                       Sub(
                           [ SingleTyping(ISimple("n", pos 5 28 103 f2), TName("Int", pos 5 32 107 f2)) ],
                           Infix(
                               Infix(
                                   VName(ASimple("n", pos 5 39 114 f2)),
                                   GreaterThanOrEqual,
                                   ValueLiteral(VInt 0, pos 5 44 119 f2)
                               ),
                               LogicalAnd,
                               Infix(
                                   VName(ASimple("n", pos 5 49 124 f2)),
                                   LessThan,
                                   VName(ASimple("max", pos 5 53 128 f2))
                               )
                           )
                       )
                   )) ]
            Value [ Typing(SingleTyping(ISimple("max", pos 7 13 161 f2), TName("Int", pos 7 19 167 f2))) ]
            AxiomDeclaration
                [ Infix(VName(ASimple("max", pos 9 13 197 f2)), Equal, ValueLiteral(VInt 5, pos 9 19 203 f2)) ]
            TransitionSystemDeclaration(
                ("TS", pos 12 14 253 f2),
                [ Variable
                      [ Typing(
                            SingleTyping(
                                IGeneric(
                                    ("position", pos 14 17 294 f2),
                                    [ SingleTyping(ISimple("t", pos 14 28 305 f2), TName("TrainId", pos 14 32 309 f2)) ]
                                ),
                                TName("SegmentId", pos 14 44 321 f2)
                            )
                        )
                        Typing(
                            SingleTyping(
                                IGeneric(
                                    ("occupied", pos 15 17 348 f2),
                                    [ SingleTyping(ISimple("s", pos 15 28 359 f2), TName("SegmentId", pos 15 32 363 f2)) ]
                                ),
                                TName("Bool", pos 15 46 377 f2)
                            )
                        ) ]
                  InitConstraint(
                      Infix(
                          Infix(
                              VName(
                                  AGeneric(("position", pos 18 17 427 f2), [ VName(ASimple("t1", pos 18 26 436 f2)) ])
                              ),
                              Equal,
                              ValueLiteral(VInt 1, pos 18 32 442 f2)
                          ),
                          LogicalAnd,
                          Infix(
                              Infix(
                                  VName(
                                      AGeneric(
                                          ("position", pos 19 17 463 f2),
                                          [ VName(ASimple("t2", pos 19 26 472 f2)) ]
                                      )
                                  ),
                                  Equal,
                                  ValueLiteral(VInt 4, pos 19 32 478 f2)
                              ),
                              LogicalAnd,
                              Infix(
                                  Infix(
                                      VName(
                                          AGeneric(
                                              ("occupied", pos 20 17 499 f2),
                                              [ ValueLiteral(VInt 0, pos 20 26 508 f2) ]
                                          )
                                      ),
                                      Equal,
                                      ValueLiteral(VBool false, pos 20 31 513 f2)
                                  ),
                                  LogicalAnd,
                                  Infix(
                                      Infix(
                                          VName(
                                              AGeneric(
                                                  ("occupied", pos 21 17 538 f2),
                                                  [ ValueLiteral(VInt 1, pos 21 26 547 f2) ]
                                              )
                                          ),
                                          Equal,
                                          ValueLiteral(VBool true, pos 21 31 552 f2)
                                      ),
                                      LogicalAnd,
                                      Infix(
                                          Infix(
                                              VName(
                                                  AGeneric(
                                                      ("occupied", pos 22 17 576 f2),
                                                      [ ValueLiteral(VInt 2, pos 22 26 585 f2) ]
                                                  )
                                              ),
                                              Equal,
                                              ValueLiteral(VBool false, pos 22 31 590 f2)
                                          ),
                                          LogicalAnd,
                                          Infix(
                                              Infix(
                                                  VName(
                                                      AGeneric(
                                                          ("occupied", pos 23 17 615 f2),
                                                          [ ValueLiteral(VInt 3, pos 23 26 624 f2) ]
                                                      )
                                                  ),
                                                  Equal,
                                                  ValueLiteral(VBool false, pos 23 31 629 f2)
                                              ),
                                              LogicalAnd,
                                              Infix(
                                                  VName(
                                                      AGeneric(
                                                          ("occupied", pos 24 17 654 f2),
                                                          [ ValueLiteral(VInt 4, pos 24 26 663 f2) ]
                                                      )
                                                  ),
                                                  Equal,
                                                  ValueLiteral(VBool true, pos 24 31 668 f2)
                                              )
                                          )
                                      )
                                  )
                              )
                          )
                      )
                  )
                  TransitionRule(
                      VName(ASimple("MOVE_RIGHT", pos 27 17 731 f2)),
                      [ (("MOVE_RIGHT", pos 30 19 783 f2),
                         Quantified(
                             (Quantifier.NonDeterministic, pos 31 23 820 f2),
                             [ SingleTyping(ISimple("s1", pos 31 40 837 f2), TName("SegmentId", pos 31 45 842 f2))
                               SingleTyping(ISimple("t", pos 31 27 824 f2), TName("TrainId", pos 31 31 828 f2))
                               SingleTyping(ISimple("s2", pos 31 56 853 f2), TName("SegmentId", pos 31 61 858 f2)) ],
                             Infix(
                                 Infix(
                                     Infix(
                                         VName(
                                             AGeneric(
                                                 ("position", pos 32 25 896 f2),
                                                 [ VName(ASimple("t", pos 32 34 905 f2)) ]
                                             )
                                         ),
                                         LessThan,
                                         Infix(
                                             VName(ASimple("max", pos 32 40 911 f2)),
                                             Minus,
                                             ValueLiteral(VInt 1, pos 32 46 917 f2)
                                         )
                                     ),
                                     LogicalAnd,
                                     Infix(
                                         Infix(
                                             VName(
                                                 AGeneric(
                                                     ("position", pos 33 25 948 f2),
                                                     [ VName(ASimple("t", pos 33 34 957 f2)) ]
                                                 )
                                             ),
                                             Equal,
                                             VName(ASimple("s1", pos 33 39 962 f2))
                                         ),
                                         LogicalAnd,
                                         Infix(
                                             Infix(
                                                 Infix(
                                                     VName(ASimple("s1", pos 34 26 993 f2)),
                                                     Plus,
                                                     ValueLiteral(VInt 1, pos 34 31 998 f2)
                                                 ),
                                                 Equal,
                                                 VName(ASimple("s2", pos 34 36 1003 f2))
                                             ),
                                             LogicalAnd,
                                             LogicalNegation(
                                                 VName(
                                                     AGeneric(
                                                         ("occupied", pos 35 26 1034 f2),
                                                         [ VName(ASimple("s2", pos 35 35 1043 f2)) ]
                                                     )
                                                 ),
                                                 pos 35 25 1033 f2
                                             )
                                         )
                                     )
                                 ),
                                 Guard,
                                 VeList
                                     [ Infix(
                                           VPName(
                                               AGeneric(
                                                   ("position", pos 36 29 1080 f2),
                                                   [ VName(ASimple("t", pos 36 39 1090 f2)) ]
                                               )
                                           ),
                                           Equal,
                                           Infix(
                                               VName(
                                                   AGeneric(
                                                       ("position", pos 36 44 1095 f2),
                                                       [ VName(ASimple("t", pos 36 53 1104 f2)) ]
                                                   )
                                               ),
                                               Plus,
                                               ValueLiteral(VInt 1, pos 36 58 1109 f2)
                                           )
                                       )
                                       Infix(
                                           VPName(
                                               AGeneric(
                                                   ("occupied", pos 37 29 1140 f2),
                                                   [ VName(ASimple("s1", pos 37 39 1150 f2)) ]
                                               )
                                           ),
                                           Equal,
                                           ValueLiteral(VBool false, pos 37 45 1156 f2)
                                       )
                                       Infix(
                                           VPName(
                                               AGeneric(
                                                   ("occupied", pos 38 29 1191 f2),
                                                   [ VName(ASimple("s2", pos 38 39 1201 f2)) ]
                                               )
                                           ),
                                           Equal,
                                           ValueLiteral(VBool true, pos 38 45 1207 f2)
                                       ) ]
                             )
                         ))
                        (("MOVE_LEFT", pos 40 19 1234 f2),
                         Quantified(
                             (Quantifier.NonDeterministic, pos 41 23 1270 f2),
                             [ SingleTyping(ISimple("s1", pos 41 40 1287 f2), TName("SegmentId", pos 41 45 1292 f2))
                               SingleTyping(ISimple("t", pos 41 27 1274 f2), TName("TrainId", pos 41 31 1278 f2))
                               SingleTyping(ISimple("s2", pos 41 56 1303 f2), TName("SegmentId", pos 41 61 1308 f2)) ],
                             Infix(
                                 Infix(
                                     Infix(
                                         VName(
                                             AGeneric(
                                                 ("position", pos 42 25 1346 f2),
                                                 [ VName(ASimple("t", pos 42 34 1355 f2)) ]
                                             )
                                         ),
                                         GreaterThan,
                                         ValueLiteral(VInt 0, pos 42 39 1360 f2)
                                     ),
                                     LogicalAnd,
                                     Infix(
                                         Infix(
                                             VName(
                                                 AGeneric(
                                                     ("position", pos 43 25 1390 f2),
                                                     [ VName(ASimple("t", pos 43 34 1399 f2)) ]
                                                 )
                                             ),
                                             Equal,
                                             VName(ASimple("s1", pos 43 39 1404 f2))
                                         ),
                                         LogicalAnd,
                                         Infix(
                                             Infix(
                                                 Infix(
                                                     VName(ASimple("s1", pos 44 26 1435 f2)),
                                                     Minus,
                                                     ValueLiteral(VInt 1, pos 44 31 1440 f2)
                                                 ),
                                                 Equal,
                                                 VName(ASimple("s2", pos 44 36 1445 f2))
                                             ),
                                             LogicalAnd,
                                             LogicalNegation(
                                                 VName(
                                                     AGeneric(
                                                         ("occupied", pos 45 26 1476 f2),
                                                         [ VName(ASimple("s2", pos 45 35 1485 f2)) ]
                                                     )
                                                 ),
                                                 pos 45 25 1475 f2
                                             )
                                         )
                                     )
                                 ),
                                 Guard,
                                 VeList
                                     [ Infix(
                                           VPName(
                                               AGeneric(
                                                   ("position", pos 46 29 1522 f2),
                                                   [ VName(ASimple("t", pos 46 39 1532 f2)) ]
                                               )
                                           ),
                                           Equal,
                                           Infix(
                                               VName(
                                                   AGeneric(
                                                       ("position", pos 46 44 1537 f2),
                                                       [ VName(ASimple("t", pos 46 53 1546 f2)) ]
                                                   )
                                               ),
                                               Minus,
                                               ValueLiteral(VInt 1, pos 46 58 1551 f2)
                                           )
                                       )
                                       Infix(
                                           VPName(
                                               AGeneric(
                                                   ("occupied", pos 47 29 1582 f2),
                                                   [ VName(ASimple("s1", pos 47 39 1592 f2)) ]
                                               )
                                           ),
                                           Equal,
                                           ValueLiteral(VBool false, pos 47 45 1598 f2)
                                       )
                                       Infix(
                                           VPName(
                                               AGeneric(
                                                   ("occupied", pos 48 29 1633 f2),
                                                   [ VName(ASimple("s2", pos 48 39 1643 f2)) ]
                                               )
                                           ),
                                           Equal,
                                           ValueLiteral(VBool true, pos 48 45 1649 f2)
                                       ) ]
                             )
                         )) ]
                  ) ]
            ) ]) |] ]

[<TestCaseSource(nameof input)>]
let TestValueNat (source: string) expected =
    let actual = testLexerAndParserFromFile source

    match actual with
    | Some t -> Assert.AreEqual(expected, t)
    | None -> Assert.Fail "Should succeed"
