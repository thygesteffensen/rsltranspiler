module TranspilerTest.Parser.UnfoldValueExpression

open NUnit.Framework
open Transpiler.Ast
open Transpiler.Intermediate
open Transpiler.Helpers.Helpers
open Transpiler.RuleCollection.TypeRule
open TranspilerTest.Common

let dummyPos = pos 1 2 3 "temp.rsl"

let input: obj[] list =
    [
      (* Test that value names are replaced in the guard *)
      [| Infix(
             Infix(
                 VName(AGeneric(("position", dummyPos), [ VName(ASimple("t1", dummyPos)) ])),
                 GreaterThan,
                 VName(ASimple("s1", dummyPos))
             ),
             Guard,
             Infix(
                 VPName(AGeneric(("occupied", dummyPos), [ VName(ASimple("s1", dummyPos)) ])),
                 Equal,
                 ValueLiteral(VInt 5, dummyPos)
             )
         )
         Infix(
             Infix(VName(ASimple("position_t1", dummyPos)), GreaterThan, ValueLiteral(VInt 5, dummyPos)),
             Guard,
             Infix(VPName(ASimple("occupied_5", dummyPos)), Equal, ValueLiteral(VInt 5, dummyPos))
         )
         Map.empty.Add("T", (Union [ ("t1", dummyPos); ("t2", dummyPos) ], [VText "t1"; VText "t2"]))
         Map.empty.Add("s1", VInt 5)
         Map.empty.Add("", Literal TInt) |]

      (* Test that value expression are properly grouped *)
      [| Infix(
             Infix(
                 Infix(VName(ASimple("var_a", dummyPos)), Equal, ValueLiteral(VInt 5, dummyPos)),
                 LogicalAnd,
                 Infix(
                     Infix(VName(ASimple("var_b", dummyPos)), Equal, ValueLiteral(VInt 5, dummyPos)),
                     LogicalAnd,
                     Infix(VName(ASimple("var_c", dummyPos)), Equal, ValueLiteral(VInt 5, dummyPos))
                 )
             ),
             Guard,
             Infix(VPName(ASimple("occupied_s1", dummyPos)), Equal, ValueLiteral(VInt 5, dummyPos))
         )
         Infix(
             Infix(
                 Infix(VName(ASimple("var_a", dummyPos)), Equal, ValueLiteral(VInt 5, dummyPos)),
                 LogicalAnd,
                 Infix(
                     Infix(VName(ASimple("var_b", dummyPos)), Equal, ValueLiteral(VInt 5, dummyPos)),
                     LogicalAnd,
                     Infix(VName(ASimple("var_c", dummyPos)), Equal, ValueLiteral(VInt 5, dummyPos))
                 )
             ),
             Guard,
             Infix(VPName(ASimple("occupied_s1", dummyPos)), Equal, ValueLiteral(VInt 5, dummyPos))
         )
         Map.empty.Add("T", (Union [ ("t1", dummyPos); ("t2", dummyPos) ], [VText "t1"; VText "t2"]))
         Map.empty.Add("", VInt 1)
         Map.empty.Add("", Literal TInt) |]
      [| Prefix(
             (Globally, dummyPos),
             ValueExpression.Quantified(
                 (All, dummyPos),
                 [ SingleTyping(ISimple("t", dummyPos), TName("T", dummyPos)) ],
                 Infix(
                     VName(AGeneric(("v", dummyPos), [ VName(ASimple("t", dummyPos)) ])),
                     Equal,
                     ValueLiteral(VInt 2, dummyPos)
                 )
             )
         )
         Prefix(
             (Globally, dummyPos),
             Infix(
                 Infix(
                     VName(ASimple(("v_t1", dummyPos))),
                     Equal,
                     ValueLiteral(VInt 2, dummyPos)
                 ),
                 LogicalAnd,
                 Infix(
                     VName(ASimple(("v_t2", dummyPos))),
                     Equal,
                     ValueLiteral(VInt 2, dummyPos)
                 )
             )
         )
         Map.empty.Add("T", (Union [ ("t1", dummyPos); ("t2", dummyPos) ], [VText "t1"; VText "t2"]))
         Map.empty.Add("", VInt 1)
         Map.empty.Add("", Literal TInt)|] ]

[<TestCaseSource(nameof input)>]
let UnfoldValueExpressionTest
    (input: ValueExpression)
    (expected: ValueExpression)
    (typeEnv: TypeEnvMap)
    (valueEnv: ValueEnvMap)
    (valueTypeEnv: Map<string, TypeExpression>)
    =
    let unfolded = unfoldValueExpression typeEnv valueTypeEnv valueEnv input

    Assert.AreEqual(expected, unfolded)
