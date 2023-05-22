module TranspilerTest.Logic.TestIntermediateRepresentation

open NUnit.Framework
open Transpiler
open TranspilerTest.Common

[<SetUp>]
let Setup () = ()

let filename = ""

let testInput: obj[] list =
    [ [| [ TypeDeclaration([ ("T1", pos 1 2 3 filename), Abstract; ("T2", pos 1 2 3 filename), Concrete(TName ("Nat", pos 1 2 3 filename)) ])
           Value([ ExplicitValue(ISimple("v1", pos 1 2 3 filename), TName ("T1", pos 1 2 3 filename), ValueLiteral((VInt 123, pos 1 2 3 filename))) ])
           AxiomDeclaration([ Infix(VName(ASimple ("v1", pos 1 2 3 filename)), Equal, ValueLiteral((VInt 2, pos 1 2 3 filename))) ]) ] |]
      [| [ TypeDeclaration([ ("T1", pos 1 2 3 filename), Abstract; ("T2", pos 1 2 3 filename), Concrete(TName ("Nat", pos 1 2 3 filename)) ])
           Value(
               [ ExplicitValue(ISimple("v1", pos 1 2 3 filename), TName ("T1", pos 1 2 3 filename), ValueLiteral((VInt 123, pos 1 2 3 filename)))
                 ExplicitValue(ISimple("v2", pos 1 2 3 filename), TName ("T1", pos 1 2 3 filename), ValueLiteral((VInt 123, pos 1 2 3 filename))) ]
           )
           AxiomDeclaration([ Infix(VName(ASimple ("v1", pos 1 2 3 filename)), Equal, ValueLiteral((VInt 2, pos 1 2 3 filename))) ]) ] |] ]

[<TestCaseSource(nameof testInput)>]
let testAst2Ir2Ast decl =
    let t: Intermediate =
        { Type = None
          Value = None
          Axiom = None
          TransitionSystem = None }

    Helpers.convertToIntermediate decl t |> ignore
    let actual = Helpers.convertToAst (Helpers.convertToIntermediate decl t) []

    Assert.AreEqual(decl, actual)
