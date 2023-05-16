module TranspilerTest.Parser.ParserTest

open NUnit.Framework
open Transpiler
open TranspilerTest.Common


[<SetUp>]
let setup () = ()


let generate name rhs =
    Scheme(name, [ Value([ ExplicitValue("a", TName "Nat", rhs) ]) ])


let textInput: obj[] list =
    [ [| "Samples/Precedence.rsl"
         generate
             "Precedence"
             (Infix(Infix(ValueLiteral(VBool true), Equal, ValueLiteral(VBool true)), Guard, ValueLiteral(VBool false))) |]
      [| "Samples/Precedence2.rsl"
         generate
             "Precedence2"
             (Infix(Infix(ValueLiteral(VInt 1), Plus, ValueLiteral(VInt 1)), Guard, ValueLiteral(VBool false))) |]
      [| "Samples/Precedence3.rsl"
         generate
             "Precedence3"
             (Infix(
                 Infix(ValueLiteral(VInt 1), Plus, ValueLiteral(VInt 1)),
                 NonDeterministic,
                 Infix(ValueLiteral(VInt 2), Plus, ValueLiteral(VInt 2))
             )) |]
      [| "Samples/Precedence4.rsl"
         generate
             "Precedence4"
             (Infix(
                 Infix(
                     ValueLiteral(VInt 1),
                     Guard,
                     Infix(VPName(Simple "v1"), Equal, Infix(VName(Simple "v3"), Plus, ValueLiteral(VInt 1)))
                 ),
                 NonDeterministic,
                 ValueLiteral(VInt 1)
             )) |]
      [| "Samples/Precedence5.rsl"
         generate
             "Precedence5"
             (Infix(Infix(VName(Simple "v3"), Plus, ValueLiteral(VInt 1)), NonDeterministic, ValueLiteral(VInt 1))) |]
      [| "Samples/Precedence6.rsl"
         generate
             "Precedence6"
             (Infix(
                 Infix(VPName(Simple "v1"), Equal, Infix(VName(Simple "v3"), Plus, ValueLiteral(VInt 1))),
                 NonDeterministic,
                 ValueLiteral(VInt 1)
             )) |]
      [| "Samples/Precedence7.rsl"
         generate
             "Precedence7"
             (Quantified(
                 Quantifier.NonDeterministic,
                 [ SingleTyping("t", TName "Pos") ],
                 Infix(
                     Infix(ValueLiteral(VBool false), Equal, ValueLiteral(VBool false)),
                     Guard,
                     Infix(
                         VPName(Generic("v2", [ VName(Simple "t") ])),
                         Equal,
                         Infix(VName(Generic("v2", [ VName(Simple "t") ])), Plus, ValueLiteral(VInt 1))
                     )
                 )
             )) |] ]


[<TestCaseSource(nameof textInput)>]
let test input expected =
    let actual = testLexerAndParserFromFile input

    match actual with
    | Some t -> Assert.AreEqual(expected, t)
    | None -> Assert.Fail "Should succeed"
