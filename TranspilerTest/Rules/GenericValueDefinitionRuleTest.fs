module TranspilerTest.Rules.GenericValueDefinitionRuleTest

open NUnit.Framework
open Transpiler.Ast
open Transpiler.Intermediate
open Transpiler.RuleCollection.GenericValueDefinitionRule
open TranspilerTest.Common

let dp = pos 1 2 3 "temp.rsl"

let unfoldGenericAccessTestInput: obj[] list =
    [ [| Map.empty.Add("T", (Abstract, [ VText "t1"; VText "t2" ]))
         Map.empty.Add("x", VInt 2)
         Typing(SingleTyping(ISimple("max", dp), TName("Nat", dp)))
         [ Typing(SingleTyping(ISimple("max", dp), TName("Nat", dp))) ] |]
      [| Map.empty.Add("T", (Abstract, [ VText "t1"; VText "t2" ]))
         Map.empty.Add("x", VInt 2)
         ExplicitValue(ISimple("max", dp), TName("Nat", dp), ValueLiteral(VNat 2, dp))
         [ ExplicitValue(ISimple("max", dp), TName("Nat", dp), ValueLiteral(VNat 2, dp)) ] |]
      [| Map.empty.Add("T", (Abstract, [ VText "t1"; VText "t2" ]))
         Map.empty.Add("x", VInt 2)
         Typing(
             SingleTyping(
                 IGeneric(("position", dp), [ (SingleTyping(ISimple("t", dp), TName("T", dp))) ]),
                 TName("Nat", dp)
             )
         )
         [ Typing(SingleTyping(ISimple("position_t1", dp), TName("Nat", dp)))
           Typing(SingleTyping(ISimple("position_t2", dp), TName("Nat", dp))) ] |]
      [| Map.empty.Add("T", (Abstract, [ VText "t1"; VText "t2" ]))
         Map.empty.Add("x", VInt 2)
         ExplicitValue(
             IGeneric(("position", dp), [ (SingleTyping(ISimple("t", dp), TName("T", dp))) ]),
             TName("Nat", dp),
             ValueLiteral(VNat 2, dp)
         )
         [ ExplicitValue(ISimple("position_t1", dp), TName("Nat", dp), ValueLiteral(VNat 2, dp))
           ExplicitValue(ISimple("position_t2", dp), TName("Nat", dp), ValueLiteral(VNat 2, dp)) ] |]
      [| Map.empty.Add("T", (Abstract, [ VText "t1"; VText "t2" ]))
         Map.empty.Add("x", VInt 2)
         ExplicitValue(
             IGeneric(("position", dp), [ (SingleTyping(ISimple("t", dp), TName("T", dp))) ]),
             TName("Nat", dp),
             VName(ASimple("t", dp))
         )
         [ ExplicitValue(ISimple("position_t1", dp), TName("Nat", dp), ValueLiteral(VText "t1", dp))
           ExplicitValue(ISimple("position_t2", dp), TName("Nat", dp), ValueLiteral(VText "t2", dp)) ] |] ]

[<TestCaseSource(nameof unfoldGenericAccessTestInput)>]
let unfoldGenericAccessTest
    (typeEnv: TypeEnvMap)
    (valueEnv: ValueEnvMap)
    (inputValueDeclaration: ValueDeclaration)
    (expectedSValueDeclaration: ValueDeclaration list)
    =
    let unfoldedValueDeclaration =
        valueDeclarationFolder typeEnv valueEnv inputValueDeclaration []

    Assert.AreEqual(expectedSValueDeclaration.Length, unfoldedValueDeclaration.Length, "List must be of equal length")

    List.zip expectedSValueDeclaration unfoldedValueDeclaration
    |> List.iter Assert.AreEqual
