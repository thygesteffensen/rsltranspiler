module TranspilerTest.Parser.ParserTest

open NUnit.Framework
open Transpiler.Ast
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
             )) |] ]


[<TestCaseSource(nameof textInput)>]
let test input expected =
    let actual = testLexerAndParserFromFile input

    match actual with
    | Some t -> Assert.AreEqual(expected, t)
    | None -> Assert.Fail "Should succeed"
