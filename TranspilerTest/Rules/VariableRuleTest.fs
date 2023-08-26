module TranspilerTest.Rules.VariableRuleTest

open NUnit.Framework
open Transpiler.Ast
open Transpiler.RuleCollection.VariableRule
open TranspilerTest.Common

let dp = pos 1 2 3 "temp.rsl"

type valueDeclarationFolderTestInputType =
    { ValueDecl: ValueDeclaration
      Acc: ValueDeclaration list * ValueExpression list
      Expected: ValueDeclaration list * ValueExpression list }

let valueDeclarationFolderTestInput: valueDeclarationFolderTestInputType list =
    [ { ValueDecl = Typing(SingleTyping(ISimple("a", dp), TName("1", dp)))
        Acc = ([], [])
        Expected = ([ Typing(SingleTyping(ISimple("a", dp), TName("1", dp))) ], []) }

      { ValueDecl = Typing(SingleTyping(ISimple("a", dp), TName("1", dp)))
        Acc =
          ([ Typing(SingleTyping(ISimple("b", dp), TName("2", dp))) ],
           [ Infix(VName(ASimple("position_t1", dp)), Equal, ValueLiteral(VInt 2, dp)) ])
        Expected =
          ([ Typing(SingleTyping(ISimple("a", dp), TName("1", dp)))
             Typing(SingleTyping(ISimple("b", dp), TName("2", dp))) ],
           [ Infix(VName(ASimple("position_t1", dp)), Equal, ValueLiteral(VInt 2, dp)) ]) }

      { ValueDecl = ExplicitValue(ISimple("n", dp), TName("Nat", dp), ValueLiteral(VNat 2, dp))
        Acc = ([], [])
        Expected =
          ([ Typing(SingleTyping(ISimple("n", dp), TName("Nat", dp))) ],
           [ Infix(VName(ASimple("n", dp)), Equal, ValueLiteral(VNat 2, dp)) ]) }

      { ValueDecl = ExplicitValue(ISimple("n", dp), TName("Nat", dp), ValueLiteral(VNat 2, dp))
        Acc =
          ([ Typing(SingleTyping(ISimple("b", dp), TName("2", dp))) ],
           [ Infix(VName(ASimple("position_t1", dp)), Equal, ValueLiteral(VInt 2, dp)) ])
        Expected =
          ([ Typing(SingleTyping(ISimple("n", dp), TName("Nat", dp)))
             Typing(SingleTyping(ISimple("b", dp), TName("2", dp))) ],
           [ Infix(VName(ASimple("n", dp)), Equal, ValueLiteral(VNat 2, dp))
             Infix(VName(ASimple("position_t1", dp)), Equal, ValueLiteral(VInt 2, dp)) ]) } ]

[<TestCaseSource(nameof valueDeclarationFolderTestInput)>]
let valueDeclarationFolderTest (input: valueDeclarationFolderTestInputType) =
    let valueDeclsAc, valueExprsAc = valueDeclarationFolder input.ValueDecl input.Acc

    Assert.That(valueDeclsAc, Is.EquivalentTo(fst input.Expected))
    Assert.That(valueExprsAc, Is.EquivalentTo(snd input.Expected))
