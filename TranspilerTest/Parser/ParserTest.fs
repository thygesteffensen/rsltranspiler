module TranspilerTest.Parser.ParserTest

open NUnit.Framework
open Transpiler.Ast
open Transpiler.Reader
open TranspilerTest.Common


[<SetUp>]
let setup () = ()


let generate2 name rhs =
    Scheme(
        (name, pos 1 8 7 $"{name}.rsl"),
        [ TransitionSystemDeclaration(("TS", pos 3 28 59 $"{name}.rsl"), [ TransitionRule(rhs, []) ]) ]
    )


let textInput: obj[] list =
    [ [| "Samples/Precedence0.rsl"
         generate2
             "Precedence0"
             (Infix(
                 Infix(
                     ValueLiteral((VBool true, pos 5 17 108 "Precedence0.rsl")),
                     Equal,
                     ValueLiteral((VBool true, pos 5 24 115 "Precedence0.rsl"))
                 ),
                 Guard,
                 VeList [ ValueLiteral((VBool false, pos 5 33 124 "Precedence0.rsl")) ]
             )) |]
      [| "Samples/Precedence4.rsl"
         generate2
             "Precedence4"
             (Infix(
                 Infix(
                     ValueLiteral((VBool false, pos 5 17 108 "Precedence4.rsl")),
                     Guard,
                     VeList
                         [ Infix(
                               VPName(ASimple("v1", pos 5 27 118 "Precedence4.rsl")),
                               Equal,
                               Infix(
                                   VName(ASimple("v3", pos 5 33 124 "Precedence4.rsl")),
                                   Plus,
                                   ValueLiteral((VInt 1, pos 5 38 129 "Precedence4.rsl"))
                               )
                           ) ]
                 ),
                 NonDeterministic,
                 Infix(
                     ValueLiteral(VBool true, pos 5 44 135 "Precedence4.rsl"),
                     Guard,
                     VeList
                         [ Infix(
                               VPName(ASimple("v2", pos 5 53 144 "Precedence4.rsl")),
                               Equal,
                               ValueLiteral((VInt 2, pos 5 59 150 "Precedence4.rsl"))
                           ) ]
                 )
             )) |]
      [| "Samples/Precedence7.rsl"
         generate2
             "Precedence7"
             (Quantified(
                 (Quantifier.NonDeterministic, pos 5 18 109 "Precedence7.rsl"),
                 [ SingleTyping(
                       ISimple("t", pos 5 22 113 "Precedence7.rsl"),
                       TName("Pos", pos 5 26 117 "Precedence7.rsl")
                   ) ],
                 Infix(
                     Infix(
                         ValueLiteral((VBool false, pos 5 33 124 "Precedence7.rsl")),
                         Equal,
                         ValueLiteral((VBool false, pos 5 41 132 "Precedence7.rsl"))
                     ),
                     Guard,
                     VeList
                         [ Infix(
                               VPName(
                                   AGeneric(
                                       ("v2", pos 5 51 142 "Precedence7.rsl"),
                                       [ VName(ASimple("t", pos 5 55 146 "Precedence7.rsl")) ]
                                   )
                               ),
                               Equal,
                               Infix(
                                   VName(
                                       AGeneric(
                                           ("v2", pos 5 60 151 "Precedence7.rsl"),
                                           [ VName(ASimple("t", pos 5 63 154 "Precedence7.rsl")) ]
                                       )
                                   ),
                                   Plus,
                                   ValueLiteral((VInt 1, pos 5 68 159 "Precedence7.rsl"))
                               )
                           ) ]
                 )
             )) |]
      [| "Samples/Precedence8.rsl"
         generate2
             "Precedence8"
             (Infix(
                 Infix(
                     ValueLiteral((VBool false, pos 5 17 108 "Precedence8.rsl")),
                     Guard,
                     VeList
                         [ Infix(
                               VPName(ASimple("v1", pos 5 27 118 "Precedence8.rsl")),
                               Equal,
                               Infix(
                                   VName(ASimple("v3", pos 5 33 124 "Precedence8.rsl")),
                                   Plus,
                                   ValueLiteral((VInt 1, pos 5 38 129 "Precedence8.rsl"))
                               )
                           )
                           Infix(
                               VPName(ASimple("v2", pos 5 41 132 "Precedence8.rsl")),
                               Equal,
                               ValueLiteral(VInt 2, pos 5 47 138 "Precedence8.rsl")
                           ) ]
                 ),
                 NonDeterministic,
                 Infix(
                     ValueLiteral(VBool true, pos 5 53 144 "Precedence8.rsl"),
                     Guard,
                     VeList
                         [ Infix(
                               VPName(ASimple("v2", pos 5 62 153 "Precedence8.rsl")),
                               Equal,
                               ValueLiteral((VInt 2, pos 5 68 159 "Precedence8.rsl"))
                           ) ]
                 )
             )) |]
      [| "Samples/SimpleRail2.rsl"
         (("SimpleRail2", pos 1 8 7 "SimpleRail2.rsl"),
          [ TypeDeclaration
                [ (("TrainId", pos 4 13 57 "SimpleRail2.rsl"),
                   Union [ ("t1", pos 4 24 68 "SimpleRail2.rsl"); ("t2", pos 4 29 73 "SimpleRail2.rsl") ])
                  (("SegmentId", pos 5 13 89 "SimpleRail2.rsl"),
                   Concrete(
                       Sub(
                           [ SingleTyping(
                                 ISimple("n", pos 5 28 104 "SimpleRail2.rsl"),
                                 TName("Int", pos 5 32 108 "SimpleRail2.rsl")
                             ) ],
                           Infix(
                               Infix(
                                   VName(ASimple("n", pos 5 39 115 "SimpleRail2.rsl")),
                                   GreaterThanOrEqual,
                                   ValueLiteral(VInt 0, pos 5 44 120 "SimpleRail2.rsl")
                               ),
                               LogicalAnd,
                               Infix(
                                   VName(ASimple("n", pos 5 49 125 "SimpleRail2.rsl")),
                                   LessThan,
                                   VName(ASimple("max", pos 5 53 129 "SimpleRail2.rsl"))
                               )
                           )
                       )
                   )) ]
            Value
                [ Typing(
                      SingleTyping(
                          ISimple("max", pos 7 13 162 "SimpleRail2.rsl"),
                          TName("Int", pos 7 19 168 "SimpleRail2.rsl")
                      )
                  ) ]
            AxiomDeclaration
                [ Infix(
                      VName(ASimple("max", pos 9 13 198 "SimpleRail2.rsl")),
                      Equal,
                      ValueLiteral(VInt 5, pos 9 19 204 "SimpleRail2.rsl")
                  ) ]
            TransitionSystemDeclaration(
                ("TS", pos 12 14 254 "SimpleRail2.rsl"),
                [ Variable
                      [ Typing(
                            SingleTyping(
                                IGeneric(
                                    ("position", pos 14 17 295 "SimpleRail2.rsl"),
                                    [ SingleTyping(
                                          ISimple("t", pos 14 28 306 "SimpleRail2.rsl"),
                                          TName("TrainId", pos 14 32 310 "SimpleRail2.rsl")
                                      ) ]
                                ),
                                TName("SegmentId", pos 14 44 322 "SimpleRail2.rsl")
                            )
                        )
                        Typing(
                            SingleTyping(
                                IGeneric(
                                    ("occupied", pos 15 17 349 "SimpleRail2.rsl"),
                                    [ SingleTyping(
                                          ISimple("s", pos 15 28 360 "SimpleRail2.rsl"),
                                          TName("SegmentId", pos 15 32 364 "SimpleRail2.rsl")
                                      ) ]
                                ),
                                TName("Bool", pos 15 46 378 "SimpleRail2.rsl")
                            )
                        ) ]
                  InitConstraint(
                      Infix(
                          Infix(
                              VName(
                                  AGeneric(
                                      ("position", pos 18 17 428 "SimpleRail2.rsl"),
                                      [ VName(ASimple("t1", pos 18 26 437 "SimpleRail2.rsl")) ]
                                  )
                              ),
                              Equal,
                              ValueLiteral(VInt 1, pos 18 32 443 "SimpleRail2.rsl")
                          ),
                          LogicalAnd,
                          Infix(
                              Infix(
                                  VName(
                                      AGeneric(
                                          ("position", pos 19 17 464 "SimpleRail2.rsl"),
                                          [ VName(ASimple("t2", pos 19 26 473 "SimpleRail2.rsl")) ]
                                      )
                                  ),
                                  Equal,
                                  ValueLiteral(VInt 4, pos 19 32 479 "SimpleRail2.rsl")
                              ),
                              LogicalAnd,
                              Infix(
                                  Infix(
                                      VName(
                                          AGeneric(
                                              ("occupied", pos 20 17 500 "SimpleRail2.rsl"),
                                              [ ValueLiteral(VInt 0, pos 20 26 509 "SimpleRail2.rsl") ]
                                          )
                                      ),
                                      Equal,
                                      ValueLiteral(VBool false, pos 20 31 514 "SimpleRail2.rsl")
                                  ),
                                  LogicalAnd,
                                  Infix(
                                      Infix(
                                          VName(
                                              AGeneric(
                                                  ("occupied", pos 21 17 539 "SimpleRail2.rsl"),
                                                  [ ValueLiteral(VInt 1, pos 21 26 548 "SimpleRail2.rsl") ]
                                              )
                                          ),
                                          Equal,
                                          ValueLiteral(VBool true, pos 21 31 553 "SimpleRail2.rsl")
                                      ),
                                      LogicalAnd,
                                      Infix(
                                          Infix(
                                              VName(
                                                  AGeneric(
                                                      ("occupied", pos 22 17 577 "SimpleRail2.rsl"),
                                                      [ ValueLiteral(VInt 2, pos 22 26 586 "SimpleRail2.rsl") ]
                                                  )
                                              ),
                                              Equal,
                                              ValueLiteral(VBool false, pos 22 31 591 "SimpleRail2.rsl")
                                          ),
                                          LogicalAnd,
                                          Infix(
                                              Infix(
                                                  VName(
                                                      AGeneric(
                                                          ("occupied", pos 23 17 616 "SimpleRail2.rsl"),
                                                          [ ValueLiteral(VInt 3, pos 23 26 625 "SimpleRail2.rsl") ]
                                                      )
                                                  ),
                                                  Equal,
                                                  ValueLiteral(VBool false, pos 23 31 630 "SimpleRail2.rsl")
                                              ),
                                              LogicalAnd,
                                              Infix(
                                                  VName(
                                                      AGeneric(
                                                          ("occupied", pos 24 17 655 "SimpleRail2.rsl"),
                                                          [ ValueLiteral(VInt 4, pos 24 26 664 "SimpleRail2.rsl") ]
                                                      )
                                                  ),
                                                  Equal,
                                                  ValueLiteral(VBool true, pos 24 31 669 "SimpleRail2.rsl")
                                              )
                                          )
                                      )
                                  )
                              )
                          )
                      )
                  )
                  TransitionRule(
                      VName(ASimple("MOVE_RIGHT", pos 27 17 732 "SimpleRail2.rsl")),
                      [ (("MOVE_RIGHT", pos 30 19 784 "SimpleRail2.rsl"),
                         Quantified(
                             (Quantifier.NonDeterministic, pos 31 23 821 "SimpleRail2.rsl"),
                             [ SingleTyping(
                                   ISimple("s1", pos 31 40 838 "SimpleRail2.rsl"),
                                   TName("SegmentId", pos 31 45 843 "SimpleRail2.rsl")
                               )
                               SingleTyping(
                                   ISimple("t", pos 31 27 825 "SimpleRail2.rsl"),
                                   TName("TrainId", pos 31 31 829 "SimpleRail2.rsl")
                               )
                               SingleTyping(
                                   ISimple("s2", pos 31 56 854 "SimpleRail2.rsl"),
                                   TName("SegmentId", pos 31 61 859 "SimpleRail2.rsl")
                               ) ],
                             Infix(
                                 Infix(
                                     Infix(
                                         VName(
                                             AGeneric(
                                                 ("position", pos 32 25 897 "SimpleRail2.rsl"),
                                                 [ VName(ASimple("t", pos 32 34 906 "SimpleRail2.rsl")) ]
                                             )
                                         ),
                                         LessThan,
                                         Infix(
                                             VName(ASimple("max", pos 32 40 912 "SimpleRail2.rsl")),
                                             Minus,
                                             ValueLiteral(VInt 1, pos 32 46 918 "SimpleRail2.rsl")
                                         )
                                     ),
                                     LogicalAnd,
                                     Infix(
                                         Infix(
                                             VName(
                                                 AGeneric(
                                                     ("position", pos 33 25 949 "SimpleRail2.rsl"),
                                                     [ VName(ASimple("t", pos 33 34 958 "SimpleRail2.rsl")) ]
                                                 )
                                             ),
                                             Equal,
                                             VName(ASimple("s1", pos 33 39 963 "SimpleRail2.rsl"))
                                         ),
                                         LogicalAnd,
                                         Infix(
                                             Infix(
                                                 Infix(
                                                     VName(ASimple("s1", pos 34 26 994 "SimpleRail2.rsl")),
                                                     Plus,
                                                     ValueLiteral(VInt 1, pos 34 31 999 "SimpleRail2.rsl")
                                                 ),
                                                 Equal,
                                                 VName(ASimple("s2", pos 34 36 1004 "SimpleRail2.rsl"))
                                             ),
                                             LogicalAnd,
                                             LogicalNegation(
                                                 VName(
                                                     AGeneric(
                                                         ("occupied", pos 35 26 1035 "SimpleRail2.rsl"),
                                                         [ VName(ASimple("s2", pos 35 35 1044 "SimpleRail2.rsl")) ]
                                                     )
                                                 ),
                                                 pos 35 25 1034 "SimpleRail2.rsl"
                                             )
                                         )
                                     )
                                 ),
                                 Guard,
                                 VeList
                                     [ Infix(
                                           VPName(
                                               AGeneric(
                                                   ("position", pos 36 29 1081 "SimpleRail2.rsl"),
                                                   [ VName(ASimple("t", pos 36 39 1091 "SimpleRail2.rsl")) ]
                                               )
                                           ),
                                           Equal,
                                           Infix(
                                               VName(
                                                   AGeneric(
                                                       ("position", pos 36 44 1096 "SimpleRail2.rsl"),
                                                       [ VName(ASimple("t", pos 36 53 1105 "SimpleRail2.rsl")) ]
                                                   )
                                               ),
                                               Plus,
                                               ValueLiteral(VInt 1, pos 36 58 1110 "SimpleRail2.rsl")
                                           )
                                       )
                                       Infix(
                                           VPName(
                                               AGeneric(
                                                   ("occupied", pos 37 29 1141 "SimpleRail2.rsl"),
                                                   [ VName(ASimple("s1", pos 37 39 1151 "SimpleRail2.rsl")) ]
                                               )
                                           ),
                                           Equal,
                                           ValueLiteral(VBool false, pos 37 45 1157 "SimpleRail2.rsl")
                                       )
                                       Infix(
                                           VPName(
                                               AGeneric(
                                                   ("occupied", pos 38 29 1192 "SimpleRail2.rsl"),
                                                   [ VName(ASimple("s2", pos 38 39 1202 "SimpleRail2.rsl")) ]
                                               )
                                           ),
                                           Equal,
                                           ValueLiteral(VBool true, pos 38 45 1208 "SimpleRail2.rsl")
                                       ) ]
                             )
                         ))
                        (("MOVE_LEFT", pos 40 19 1235 "SimpleRail2.rsl"),
                         Quantified(
                             (Quantifier.NonDeterministic, pos 41 23 1271 "SimpleRail2.rsl"),
                             [ SingleTyping(
                                   ISimple("s1", pos 41 40 1288 "SimpleRail2.rsl"),
                                   TName("SegmentId", pos 41 45 1293 "SimpleRail2.rsl")
                               )
                               SingleTyping(
                                   ISimple("t", pos 41 27 1275 "SimpleRail2.rsl"),
                                   TName("TrainId", pos 41 31 1279 "SimpleRail2.rsl")
                               )
                               SingleTyping(
                                   ISimple("s2", pos 41 56 1304 "SimpleRail2.rsl"),
                                   TName("SegmentId", pos 41 61 1309 "SimpleRail2.rsl")
                               ) ],
                             Infix(
                                 Infix(
                                     Infix(
                                         VName(
                                             AGeneric(
                                                 ("position", pos 42 25 1347 "SimpleRail2.rsl"),
                                                 [ VName(ASimple("t", pos 42 34 1356 "SimpleRail2.rsl")) ]
                                             )
                                         ),
                                         GreaterThan,
                                         ValueLiteral(VInt 0, pos 42 39 1361 "SimpleRail2.rsl")
                                     ),
                                     LogicalAnd,
                                     Infix(
                                         Infix(
                                             VName(
                                                 AGeneric(
                                                     ("position", pos 43 25 1391 "SimpleRail2.rsl"),
                                                     [ VName(ASimple("t", pos 43 34 1400 "SimpleRail2.rsl")) ]
                                                 )
                                             ),
                                             Equal,
                                             VName(ASimple("s1", pos 43 39 1405 "SimpleRail2.rsl"))
                                         ),
                                         LogicalAnd,
                                         Infix(
                                             Infix(
                                                 Infix(
                                                     VName(ASimple("s1", pos 44 26 1436 "SimpleRail2.rsl")),
                                                     Minus,
                                                     ValueLiteral(VInt 1, pos 44 31 1441 "SimpleRail2.rsl")
                                                 ),
                                                 Equal,
                                                 VName(ASimple("s2", pos 44 36 1446 "SimpleRail2.rsl"))
                                             ),
                                             LogicalAnd,
                                             LogicalNegation(
                                                 VName(
                                                     AGeneric(
                                                         ("occupied", pos 45 26 1477 "SimpleRail2.rsl"),
                                                         [ VName(ASimple("s2", pos 45 35 1486 "SimpleRail2.rsl")) ]
                                                     )
                                                 ),
                                                 pos 45 25 1476 "SimpleRail2.rsl"
                                             )
                                         )
                                     )
                                 ),
                                 Guard,
                                 VeList
                                     [ Infix(
                                           VPName(
                                               AGeneric(
                                                   ("position", pos 46 29 1523 "SimpleRail2.rsl"),
                                                   [ VName(ASimple("t", pos 46 39 1533 "SimpleRail2.rsl")) ]
                                               )
                                           ),
                                           Equal,
                                           Infix(
                                               VName(
                                                   AGeneric(
                                                       ("position", pos 46 44 1538 "SimpleRail2.rsl"),
                                                       [ VName(ASimple("t", pos 46 53 1547 "SimpleRail2.rsl")) ]
                                                   )
                                               ),
                                               Minus,
                                               ValueLiteral(VInt 1, pos 46 58 1552 "SimpleRail2.rsl")
                                           )
                                       )
                                       Infix(
                                           VPName(
                                               AGeneric(
                                                   ("occupied", pos 47 29 1583 "SimpleRail2.rsl"),
                                                   [ VName(ASimple("s1", pos 47 39 1593 "SimpleRail2.rsl")) ]
                                               )
                                           ),
                                           Equal,
                                           ValueLiteral(VBool false, pos 47 45 1599 "SimpleRail2.rsl")
                                       )
                                       Infix(
                                           VPName(
                                               AGeneric(
                                                   ("occupied", pos 48 29 1634 "SimpleRail2.rsl"),
                                                   [ VName(ASimple("s2", pos 48 39 1644 "SimpleRail2.rsl")) ]
                                               )
                                           ),
                                           Equal,
                                           ValueLiteral(VBool true, pos 48 45 1650 "SimpleRail2.rsl")
                                       ) ]
                             )
                         )) ]
                  ) ]
            )
            LtlAssertionDeclaration
                [ (("one_train_per_section", pos 52 10 1701 "SimpleRail2.rsl"),
                   ("TS", pos 52 33 1724 "SimpleRail2.rsl"),
                   Prefix(
                       (Globally, pos 52 39 1730 "SimpleRail2.rsl"),
                       Quantified(
                           (All, pos 52 41 1732 "SimpleRail2.rsl"),
                           [ SingleTyping(
                                 ISimple("t1", pos 52 45 1736 "SimpleRail2.rsl"),
                                 TName("TrainId", pos 52 49 1740 "SimpleRail2.rsl")
                             )
                             SingleTyping(
                                 ISimple("t2", pos 52 58 1749 "SimpleRail2.rsl"),
                                 TName("TrainId", pos 52 62 1753 "SimpleRail2.rsl")
                             ) ],
                           Infix(
                               VName(ASimple("t1", pos 52 73 1764 "SimpleRail2.rsl")),
                               NotEqual,
                               Infix(
                                   VName(ASimple("t2", pos 52 79 1770 "SimpleRail2.rsl")),
                                   Implies,
                                   Infix(
                                       VName(
                                           AGeneric(
                                               ("position", pos 52 85 1776 "SimpleRail2.rsl"),
                                               [ VName(ASimple("t1", pos 52 94 1785 "SimpleRail2.rsl")) ]
                                           )
                                       ),
                                       NotEqual,
                                       VName(
                                           AGeneric(
                                               ("position", pos 52 101 1792 "SimpleRail2.rsl"),
                                               [ VName(ASimple("t2", pos 52 110 1801 "SimpleRail2.rsl")) ]
                                           )
                                       )
                                   )
                               )
                           )
                       )
                   ))
                  (("occupied_correct", pos 53 10 1816 "SimpleRail2.rsl"),
                   ("TS", pos 53 28 1834 "SimpleRail2.rsl"),
                   Prefix(
                       (Globally, pos 53 34 1840 "SimpleRail2.rsl"),
                       Quantified(
                           (All, pos 53 36 1842 "SimpleRail2.rsl"),
                           [ SingleTyping(
                                 ISimple("t", pos 53 40 1846 "SimpleRail2.rsl"),
                                 TName("TrainId", pos 53 43 1849 "SimpleRail2.rsl")
                             )
                             SingleTyping(
                                 ISimple("s", pos 53 52 1858 "SimpleRail2.rsl"),
                                 TName("SegmentId", pos 53 55 1861 "SimpleRail2.rsl")
                             ) ],
                           Infix(
                               VName(
                                   AGeneric(
                                       ("position", pos 53 68 1874 "SimpleRail2.rsl"),
                                       [ VName(ASimple("t", pos 53 77 1883 "SimpleRail2.rsl")) ]
                                   )
                               ),
                               Equal,
                               Infix(
                                   VName(ASimple("s", pos 53 82 1888 "SimpleRail2.rsl")),
                                   Implies,
                                   VName(
                                       AGeneric(
                                           ("occupied", pos 53 87 1893 "SimpleRail2.rsl"),
                                           [ VName(ASimple("s", pos 53 96 1902 "SimpleRail2.rsl")) ]
                                       )
                                   )
                               )
                           )
                       )
                   )) ] ]) |] ]


[<TestCaseSource(nameof textInput)>]
let test input expected =
    let actual = testLexerAndParserFromFile input

    match actual with
    | Some t -> Assert.AreEqual(expected, t)
    | None -> Assert.Fail "Should succeed"
