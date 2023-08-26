module TranspilerTest.Rules.GenericAccessTest

open NUnit.Framework
open Transpiler.Ast
open Transpiler.RuleCollection.GenericAccessRule
open Transpiler.Intermediate
open TranspilerTest.Common

let dp = pos 1 2 3 "temp.rsl"


let unfoldGenericAccessTestInputSimple: obj[] list =
    [ [| Map.empty.Add("T", (Abstract, [ VText "t1"; VText "t2" ]))
         Map.empty.Add("x", VInt 2)
         VName(ASimple("x", dp))
         VName(ASimple("x", dp)) |]
      [| Map.empty.Add("T", (Abstract, [ VText "t1"; VText "t2" ]))
         Map.empty.Add("t", VInt 2)
         VPName(ASimple("x", dp))
         VPName(ASimple("x", dp)) |]
      [| Map.empty.Add("T", (Abstract, [ VText "t1"; VText "t2" ]))
         Map.empty.Add("t", VText "t1")
         VName(AGeneric(("v", dp), [ VName(ASimple("t", dp)) ]))
         VName(ASimple("v_t1", dp)) |]
      [| Map.empty.Add("T", (Abstract, [ VText "t1"; VText "t2" ]))
         Map.empty.Add("t", VText "t1")
         VPName(AGeneric(("v", dp), [ VName(ASimple("t", dp)) ]))
         VPName(ASimple("v_t1", dp)) |]
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
         Prefix((Globally, dp), ValueLiteral(VBool false, dp)) |] ]


[<TestCaseSource(nameof unfoldGenericAccessTestInputSimple)>]
let unfoldGenericAccessTest
    (typeEnv: TypeEnvMap)
    (valueEnv: ValueEnvMap)
    (inputValueExpr: ValueExpression)
    (expectedSValueExpr: ValueExpression)
    =
    let unfoldedValueExpr = unfoldGenericAccess typeEnv valueEnv inputValueExpr

    Assert.AreEqual(expectedSValueExpr, unfoldedValueExpr)
