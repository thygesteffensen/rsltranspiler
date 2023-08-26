module TranspilerTest.Rules.TransitionSystemRule1Test

open NUnit.Framework

open Transpiler.Ast
open Transpiler.RuleCollection.TransitionSystemRule1

open TranspilerTest.Common

let dp = pos 1 2 3 "temp.rsl"

let exprWalkerTestInput: obj[] list =
    [ [| VName(ASimple("M1", dp))
         Map.empty.Add("M1", ValueLiteral(VInt 2, dp))
         ValueLiteral(VInt 2, dp) |]
      [| Infix(VName(ASimple("M1", dp)), NonDeterministic, VName(ASimple("M1", dp)))
         Map.empty.Add("M1", ValueLiteral(VInt 2, dp))
         Infix(ValueLiteral(VInt 2, dp), NonDeterministic, ValueLiteral(VInt 2, dp)) |]
      [| Infix(VName(ASimple("M1", dp)), NonDeterministic, VName(ASimple("x", dp)))
         Map.empty.Add("M1", ValueLiteral(VInt 2, dp))
         Infix(ValueLiteral(VInt 2, dp), NonDeterministic, VName(ASimple("x", dp))) |] ]

(*
    Hvordan skal vi håndtere nested referencer?
    
    A type checking would have concluded that no rules can be recursive and that no previous rules can be
    referenced, thus no circular references.
    
    1: walk the rule right when it replaced, this
*)

[<TestCaseSource(nameof exprWalkerTestInput)>]
let exprWalkerTest (valueExpr: ValueExpression) (ruleMap: RuleMapType) (expected: ValueExpression) =
    let actual = exprWalker valueExpr ruleMap

    Assert.AreEqual(actual, expected)


type TransitionSystemFolderTestInputType =
    { TR: TransitionSystem
      Acc: TransitionSystem list
      Expected: TransitionSystem list }

let transitionSystemFolderTestInput: TransitionSystemFolderTestInputType list =
    [ { TR =
          TransitionRule(
              VName(ASimple("M2", dp)),
              [ (("M1", dp), ValueLiteral(VInt 2, dp))
                (("M2", dp), Infix(ValueLiteral(VInt 3, dp), Plus, VName(ASimple("M1", dp)))) ]
          )
        Acc = []
        Expected = [ TransitionRule(Infix(ValueLiteral(VInt 3, dp), Plus, ValueLiteral(VInt 2, dp)), []) ] }
      { TR =
          TransitionRule(
              VName(ASimple("M2", dp)),
              [ (("M1", dp), ValueLiteral(VInt 2, dp))
                (("M2", dp), Infix(ValueLiteral(VInt 3, dp), Plus, VName(ASimple("M1", dp)))) ]
          )
        Acc = [ TransitionRule(Infix(ValueLiteral(VInt 1, dp), Minus, ValueLiteral(VInt 5, dp)), []) ]
        Expected =
          [ TransitionRule(Infix(ValueLiteral(VInt 3, dp), Plus, ValueLiteral(VInt 2, dp)), [])
            TransitionRule(Infix(ValueLiteral(VInt 1, dp), Minus, ValueLiteral(VInt 5, dp)), []) ] } ]

[<TestCaseSource(nameof transitionSystemFolderTestInput)>]
let transitionSystemFolderTest (input: TransitionSystemFolderTestInputType) =
    let actual = transitionSystemFolder input.TR input.Acc

    Assert.AreEqual(input.Expected, actual)
