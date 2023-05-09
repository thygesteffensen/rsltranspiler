module TranspilerTest.Logic.TestIntermediateRepresentation

open NUnit.Framework
open Transpiler
open TranspilerTest

[<SetUp>]
let Setup () = ()

let testInput: obj[] list =
    [ [| [ TypeDeclaration([ "T1", Abstract; "T2", Concrete(TName "Nat") ])
           Value([ ExplicitValue("v1", TName "T1", ValueLiteral(VInt 123)) ])
           AxiomDeclaration([ Equivalence(VName "v1", ValueLiteral(VInt 2)) ]) ] |]
      [| [ TypeDeclaration([ "T1", Abstract; "T2", Concrete(TName "Nat") ])
           Value(
               [ ExplicitValue("v1", TName "T1", ValueLiteral(VInt 123))
                 ExplicitValue("v2", TName "T1", ValueLiteral(VInt 123)) ]
           )
           AxiomDeclaration([ Equivalence(VName "v1", ValueLiteral(VInt 2)) ]) ] |] ]

[<TestCaseSource(nameof testInput)>]
let testAst2Ir2Ast decl =
    let t: Transpiler.Intermediate =
        { Type = None
          Value = None
          Axiom = None }

    Transpiler.convertToIntermediate decl t |> ignore
    let actual = Transpiler.convertToAst (Transpiler.convertToIntermediate decl t) []

    Assert.AreEqual(decl, actual)
