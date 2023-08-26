module TranspilerTest.Rules.QuantificationRuleTest

open NUnit.Framework
open Transpiler.Ast
open Transpiler.RuleCollection.QuantificationRule
open Transpiler.Intermediate
open TranspilerTest.Common

let dp = pos 1 2 3 "temp.rsl"


let unfoldQuantifiedTestInputSimple: obj[] list =
    [ [| Map.empty.Add("T", (Abstract, [ VText "t1"; VText "t2" ]))
         Map.empty.Add("x", VInt 2)
         VName(ASimple("y", dp))
         VName(ASimple("y", dp)) |]
      [| Map.empty.Add("T", (Abstract, [ VText "t1"; VText "t2" ]))
         Map.empty.Add("x", VInt 2)
         VName(ASimple("x", dp))
         ValueLiteral(VInt 2, dp) |]
      [| Map.empty.Add("T", (Abstract, [ VText "t1"; VText "t2" ]))
         Map.empty.Add("x", VInt 2)
         VPName(ASimple("y", dp))
         VPName(ASimple("y", dp)) |]
      [| Map.empty.Add("T", (Abstract, [ VText "t1"; VText "t2" ]))
         Map.empty.Add("x", VInt 2)
         VPName(ASimple("x", dp))
         VPName(ASimple("x", dp)) |]
      [| Map.empty.Add("T", (Abstract, [ VText "t1"; VText "t2" ]))
         Map.empty.Add("x", VInt 2)
         ValueLiteral(VInt 2, dp)
         ValueLiteral(VInt 2, dp) |]
      [| Map.empty.Add("T", (Abstract, [ VText "t1"; VText "t2" ]))
         Map.empty.Add("x", VInt 2)
         Rule("R1", dp)
         Rule("R1", dp) |]
      [| Map.empty.Add("T", (Abstract, [ VText "t1"; VText "t2" ]))
         Map.empty.Add("x", VInt 2)
         Rule("R1", dp)
         Rule("R1", dp) |]
      [| Map.empty.Add("T", (Abstract, [ VText "t1"; VText "t2" ]))
         Map.empty.Add("x", VInt 2)
         Infix(ValueLiteral(VBool true, dp), InfixOp.LogicalAnd, ValueLiteral(VBool true, dp))
         Infix(ValueLiteral(VBool true, dp), InfixOp.LogicalAnd, ValueLiteral(VBool true, dp)) |]
      [| Map.empty.Add("T", (Abstract, [ VText "t1"; VText "t2" ]))
         Map.empty.Add("x", VInt 2)
         VeList([ Rule("R1", dp); Rule("R1", dp) ])
         VeList([ Rule("R1", dp); Rule("R1", dp) ]) |]
      [| Map.empty.Add("T", (Abstract, [ VText "t1"; VText "t2" ]))
         Map.empty.Add("x", VInt 2)
         VArray([ Rule("R1", dp); Rule("R1", dp) ])
         VArray([ Rule("R1", dp); Rule("R1", dp) ]) |]
      [| Map.empty.Add("T", (Abstract, [ VText "t1"; VText "t2" ]))
         Map.empty.Add("x", VInt 2)
         LogicalNegation(ValueLiteral(VBool false, dp), dp)
         LogicalNegation(ValueLiteral(VBool false, dp), dp) |]
      [| Map.empty.Add("T", (Abstract, [ VText "t1"; VText "t2" ]))
         Map.empty.Add("x", VInt 2)
         Prefix((Globally, dp), ValueLiteral(VBool false, dp))
         Prefix((Globally, dp), ValueLiteral(VBool false, dp)) |]
      [| Map.empty.Add("T", (Abstract, [ VText "t1"; VText "t2" ]))
         Map.empty.Add("x", VInt 2)
         ValueExpression.Quantified(
             (All, dp),
             [ SingleTyping(ISimple("t", dp), TName("T", dp)) ],
             Infix(VName(AGeneric(("v", dp), [ VName(ASimple("t", dp)) ])), Equal, ValueLiteral(VInt 2, dp))
         )
         Infix(
             Infix(VName(ASimple("v_t1", dp)), Equal, ValueLiteral(VInt 2, dp)),
             LogicalAnd,
             Infix(VName(ASimple("v_t2", dp)), Equal, ValueLiteral(VInt 2, dp))
         ) |]
      [| Map.empty.Add("T", (Abstract, [ VText "t1"; VText "t2" ]))
         Map.empty.Add("x", VInt 2)
         ValueExpression.Quantified(
             (All, dp),
             [ SingleTyping(ISimple("t", dp), TName("T", dp)) ],
             Infix(VName(AGeneric(("v", dp), [ VName(ASimple("t", dp)) ])), Equal, VName(ASimple("t", dp)))
         )
         Infix(
             Infix(VName(ASimple("v_t1", dp)), Equal, ValueLiteral(VText "t1", dp)),
             LogicalAnd,
             Infix(VName(ASimple("v_t2", dp)), Equal, ValueLiteral(VText "t2", dp))
         ) |] ]

let unfoldQuantifiedTestInputNested: obj[] list =
    [ [| Map.empty.Add("T", (Abstract, [ VText "t1"; VText "t2" ]))
         Map.empty.Add("x", VInt 2)
         VeList(
             [ ValueExpression.Quantified(
                   (All, dp),
                   [ SingleTyping(ISimple("t", dp), TName("T", dp)) ],
                   Infix(VName(AGeneric(("v", dp), [ VName(ASimple("t", dp)) ])), Equal, ValueLiteral(VInt 2, dp))
               ) ]
         )
         VeList(
             [ Infix(
                   Infix(VName(ASimple("v_t1", dp)), Equal, ValueLiteral(VInt 2, dp)),
                   LogicalAnd,
                   Infix(VName(ASimple("v_t2", dp)), Equal, ValueLiteral(VInt 2, dp))
               ) ]
         ) |]
      [| Map.empty.Add("T", (Abstract, [ VText "t1"; VText "t2" ]))
         Map.empty.Add("x", VInt 2)
         VArray(
             [ ValueExpression.Quantified(
                   (All, dp),
                   [ SingleTyping(ISimple("t", dp), TName("T", dp)) ],
                   Infix(VName(AGeneric(("v", dp), [ VName(ASimple("t", dp)) ])), Equal, ValueLiteral(VInt 2, dp))
               ) ]
         )
         VArray(
             [ Infix(
                   Infix(VName(ASimple("v_t1", dp)), Equal, ValueLiteral(VInt 2, dp)),
                   LogicalAnd,
                   Infix(VName(ASimple("v_t2", dp)), Equal, ValueLiteral(VInt 2, dp))
               ) ]
         ) |] ]

[<TestCaseSource(nameof unfoldQuantifiedTestInputSimple)>]
[<TestCaseSource(nameof unfoldQuantifiedTestInputNested)>]
let unfoldQuantifiedTest
    (typeEnv: TypeEnvMap)
    (valueEnv: ValueEnvMap)
    (inputValueExpr: ValueExpression)
    (expectedSValueExpr: ValueExpression)
    =
    let unfoldedValueExpr = unfoldQuantified typeEnv valueEnv inputValueExpr

    Assert.AreEqual(expectedSValueExpr, unfoldedValueExpr)
