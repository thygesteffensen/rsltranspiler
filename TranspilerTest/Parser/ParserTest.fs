module TranspilerTest.Parser.ParserTest

open NUnit.Framework
open Transpiler
open TranspilerTest.Common


[<SetUp>]
let setup () = ()


let generate name rhs =
    Scheme(
        (name, pos 1 8 7 $"{name}.rsl"),
        [ Value(
              [ ExplicitValue(ISimple("a", pos 4 13 61 $"{name}.rsl"), TName("Nat", pos 4 16 64 $"{name}.rsl"), rhs) ]
          ) ]
    )


let textInput: obj[] list =
    [ [| "Samples/Precedence0.rsl"
         generate
             "Precedence0"
             (Infix(
                 Infix(
                     ValueLiteral((VBool true, pos 4 22 70 "Precedence0.rsl")),
                     Equal,
                     ValueLiteral((VBool true, pos 4 29 77 "Precedence0.rsl"))
                 ),
                 Guard,
                 ValueLiteral((VBool false, pos 4 38 86 "Precedence0.rsl"))
             )) |]
      [| "Samples/Precedence2.rsl"
         generate
             "Precedence2"
             (Infix(
                 Infix(
                     ValueLiteral((VInt 1, pos 4 22 70 "Precedence2.rsl")),
                     Plus,
                     ValueLiteral((VInt 1, pos 4 26 74 "Precedence2.rsl"))
                 ),
                 Guard,
                 ValueLiteral((VBool false, pos 4 32 80 "Precedence2.rsl"))
             )) |]
      [| "Samples/Precedence3.rsl"
         generate
             "Precedence3"
             (Infix(
                 Infix(
                     ValueLiteral((VInt 1, pos 4 22 70 "Precedence3.rsl")),
                     Plus,
                     ValueLiteral((VInt 1, pos 4 26 74 "Precedence3.rsl"))
                 ),
                 NonDeterministic,
                 Infix(
                     ValueLiteral((VInt 2, pos 4 32 80 "Precedence3.rsl")),
                     Plus,
                     ValueLiteral((VInt 2, pos 4 36 84 "Precedence3.rsl"))
                 )
             )) |]
      [| "Samples/Precedence4.rsl"
         generate
             "Precedence4"
             (Infix(
                 Infix(
                     ValueLiteral((VInt 1, pos 4 22 70 "Precedence4.rsl")),
                     Guard,
                     Infix(
                         VPName(ASimple("v1", pos 4 28 76 "Precedence4.rsl")),
                         Equal,
                         Infix(
                             VName(ASimple("v3", pos 4 34 82 "Precedence4.rsl")),
                             Plus,
                             ValueLiteral((VInt 1, pos 4 39 87 "Precedence4.rsl"))
                         )
                     )
                 ),
                 NonDeterministic,
                 ValueLiteral((VInt 1, pos 4 45 93 "Precedence4.rsl"))
             )) |]
      [| "Samples/Precedence5.rsl"
         generate
             "Precedence5"
             (Infix(
                 Infix(
                     VName(ASimple("v3", pos 4 22 70 "Precedence5.rsl")),
                     Plus,
                     ValueLiteral((VInt 1, pos 4 27 75 "Precedence5.rsl"))
                 ),
                 NonDeterministic,
                 ValueLiteral((VInt 1, pos 4 33 81 "Precedence5.rsl"))
             )) |]
      [| "Samples/Precedence6.rsl"
         generate
             "Precedence6"
             (Infix(
                 Infix(
                     VPName(ASimple("v1", pos 4 22 70 "Precedence6.rsl")),
                     Equal,
                     Infix(
                         VName(ASimple("v3", pos 4 28 76 "Precedence6.rsl")),
                         Plus,
                         ValueLiteral((VInt 1, pos 4 33 81 "Precedence6.rsl"))
                     )
                 ),
                 NonDeterministic,
                 ValueLiteral((VInt 1), pos 4 39 87 "Precedence6.rsl")
             )) |]
      [| "Samples/Precedence7.rsl"
         generate
             "Precedence7"
             (Quantified(
                 Quantifier.NonDeterministic,
                 [ SingleTyping(
                       ISimple("t", pos 4 27 75 "Precedence7.rsl"),
                       TName("Pos", pos 4 31 79 "Precedence7.rsl")
                   ) ],
                 Infix(
                     Infix(
                         ValueLiteral((VBool false, pos 4 38 86 "Precedence7.rsl")),
                         Equal,
                         ValueLiteral((VBool false, pos 4 46 94 "Precedence7.rsl"))
                     ),
                     Guard,
                     Infix(
                         VPName(
                             AGeneric(
                                 ("v2", pos 4 56 104 "Precedence7.rsl"),
                                 [ VName(ASimple("t", pos 4 60 108 "Precedence7.rsl")) ]
                             )
                         ),
                         Equal,
                         Infix(
                             VName(
                                 AGeneric(
                                     ("v2", pos 4 65 113 "Precedence7.rsl"),
                                     [ VName(ASimple("t", pos 4 68 116 "Precedence7.rsl")) ]
                                 )
                             ),
                             Plus,
                             ValueLiteral((VInt 1, pos 4 73 121 "Precedence7.rsl"))
                         )
                     )
                 )
             )) |] ]


[<TestCaseSource(nameof textInput)>]
let test input expected =
    let actual = testLexerAndParserFromFile input

    match actual with
    | Some t -> Assert.AreEqual(expected, t)
    | None -> Assert.Fail "Should succeed"
