module TranspilerTest.Parser.UnfoldValueExpression

open NUnit.Framework
open Transpiler.Ast
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
             Infix(VPName(ASimple("occupied_s1", dummyPos)), Equal, ValueLiteral(VInt 5, dummyPos))
         )
         Map.empty
         Map.empty.Add("s1", VInt 5)
         Map.empty
         Map.empty.Add("", "") |]

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
         Map.empty
         Map.empty.Add("", VInt 1)
         Map.empty
         Map.empty.Add("", "") |] ]

[<TestCaseSource(nameof input)>]
let UnfoldValueExpressionTest
    (input: ValueExpression)
    expected
    typeEnv
    valueEnv
    valueTypeEnv
    (instances: Map<string, string>)
    =
    let unfolded = unfoldValueExpression typeEnv valueTypeEnv instances input
    let replaced = replaceNameWithValue valueEnv unfolded

    Assert.AreEqual(expected, replaced)
