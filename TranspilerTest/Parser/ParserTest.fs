module TranspilerTest.Parser.ParserTest

open NUnit.Framework
open Transpiler.Ast
open TranspilerTest.Common


[<SetUp>]
let setup () = ()


let generate2 name rhs =
    Scheme(
        (name, pos 1 8 7 $"{name}.rsl"),
        [ TransitionSystemDeclaration(("TS", pos 3 28 61 $"{name}.rsl"), [ TransitionRule(rhs, []) ]) ]
    )


let textInput: obj[] list =
    [ [| "Samples/Precedence0.rsl"
         generate2
             "Precedence0"
             (Infix(
                 Infix(
                     ValueLiteral((VBool true, pos 5 17 112 "Precedence0.rsl")),
                     Equal,
                     ValueLiteral((VBool true, pos 5 24 119 "Precedence0.rsl"))
                 ),
                 Guard,
                 VeList [ ValueLiteral((VBool false, pos 5 33 128 "Precedence0.rsl")) ]
             )) |]
      [| "Samples/Precedence4.rsl"
         generate2
             "Precedence4"
             (Infix(
                 Infix(
                     ValueLiteral((VBool false, pos 5 17 113 "Precedence4.rsl")),
                     Guard,
                     VeList
                         [ Infix(
                               VPName(ASimple("v1", pos 5 27 123 "Precedence4.rsl")),
                               Equal,
                               Infix(
                                   VName(ASimple("v3", pos 5 33 129 "Precedence4.rsl")),
                                   Plus,
                                   ValueLiteral((VInt 1, pos 5 38 134 "Precedence4.rsl"))
                               )
                           ) ]
                 ),
                 NonDeterministic,
                 Infix(
                     ValueLiteral(VBool true, pos 5 44 140 "Precedence4.rsl"),
                     Guard,
                     VeList
                         [ Infix(
                               VPName(ASimple("v2", pos 5 53 149 "Precedence4.rsl")),
                               Equal,
                               ValueLiteral((VInt 2, pos 5 59 155 "Precedence4.rsl"))
                           ) ]
                 )
             )) |]
      [| "Samples/Precedence7.rsl"
         generate2
             "Precedence7"
             (Quantified(
                 (Quantifier.NonDeterministic, pos 5 18 114 "Precedence7.rsl"),
                 [ SingleTyping(
                       ISimple("t", pos 5 22 118 "Precedence7.rsl"),
                       TName("Pos", pos 5 26 122 "Precedence7.rsl")
                   ) ],
                 Infix(
                     Infix(
                         ValueLiteral((VBool false, pos 5 33 129 "Precedence7.rsl")),
                         Equal,
                         ValueLiteral((VBool false, pos 5 41 137 "Precedence7.rsl"))
                     ),
                     Guard,
                     VeList
                         [ Infix(
                               VPName(
                                   AGeneric(
                                       ("v2", pos 5 51 147 "Precedence7.rsl"),
                                       [ VName(ASimple("t", pos 5 55 151 "Precedence7.rsl")) ]
                                   )
                               ),
                               Equal,
                               Infix(
                                   VName(
                                       AGeneric(
                                           ("v2", pos 5 60 156 "Precedence7.rsl"),
                                           [ VName(ASimple("t", pos 5 63 159 "Precedence7.rsl")) ]
                                       )
                                   ),
                                   Plus,
                                   ValueLiteral((VInt 1, pos 5 68 164 "Precedence7.rsl"))
                               )
                           ) ]
                 )
             )) |]
      [| "Samples/Precedence8.rsl"
         generate2
             "Precedence8"
             (Infix(
                 Infix(
                     ValueLiteral((VBool false, pos 5 17 113 "Precedence8.rsl")),
                     Guard,
                     VeList
                         [ Infix(
                               VPName(ASimple("v1", pos 5 27 123 "Precedence8.rsl")),
                               Equal,
                               Infix(
                                   VName(ASimple("v3", pos 5 33 129 "Precedence8.rsl")),
                                   Plus,
                                   ValueLiteral((VInt 1, pos 5 38 134 "Precedence8.rsl"))
                               )
                           )
                           Infix(
                               VPName(ASimple("v2", pos 5 41 137 "Precedence8.rsl")),
                               Equal,
                               ValueLiteral(VInt 2, pos 5 47 143 "Precedence8.rsl")
                           ) ]
                 ),
                 NonDeterministic,
                 Infix(
                     ValueLiteral(VBool true, pos 5 53 149 "Precedence8.rsl"),
                     Guard,
                     VeList
                         [ Infix(
                               VPName(ASimple("v2", pos 5 62 158 "Precedence8.rsl")),
                               Equal,
                               ValueLiteral((VInt 2, pos 5 68 164 "Precedence8.rsl"))
                           ) ]
                 )
             )) |] ]


[<TestCaseSource(nameof textInput)>]
let test input expected =
    let actual = testLexerAndParserFromFile input

    match actual with
    | Some t -> Assert.AreEqual(expected, t)
    | None -> Assert.Fail "Should succeed"
