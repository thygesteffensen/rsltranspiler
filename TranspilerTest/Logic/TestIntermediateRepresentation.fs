module TranspilerTest.Logic.TestIntermediateRepresentation

open NUnit.Framework

open Transpiler
open Transpiler.Ast
open Transpiler.Intermediate
open Transpiler.Reader
open TranspilerTest.Common
open TranspilerTest.Compare

open Transpiler.Helpers.Helpers

[<SetUp>]
let Setup () = ()

let filename = ""

let testInputAst: obj[] list =
    [ [| [ TypeDeclaration([ ("T1", pos 1 2 3 filename), Abstract; ("T2", pos 1 2 3 filename), Concrete(TName ("Nat", pos 1 2 3 filename)) ])
           Value([ ExplicitValue(ISimple("v1", pos 1 2 3 filename), TName ("T1", pos 1 2 3 filename), ValueLiteral((VInt 123, pos 1 2 3 filename))) ])
           AxiomDeclaration([ Infix(VName(ASimple ("v1", pos 1 2 3 filename)), Equal, ValueLiteral((VInt 2, pos 1 2 3 filename))) ]) ] |]
      [| [ TypeDeclaration([ ("T1", pos 1 2 3 filename), Abstract; ("T2", pos 1 2 3 filename), Concrete(TName ("Nat", pos 1 2 3 filename)) ])
           Value(
               [ ExplicitValue(ISimple("v1", pos 1 2 3 filename), TName ("T1", pos 1 2 3 filename), ValueLiteral((VInt 123, pos 1 2 3 filename)))
                 ExplicitValue(ISimple("v2", pos 1 2 3 filename), TName ("T1", pos 1 2 3 filename), ValueLiteral((VInt 123, pos 1 2 3 filename))) ]
           )
           AxiomDeclaration([ Infix(VName(ASimple ("v1", pos 1 2 3 filename)), Equal, ValueLiteral((VInt 2, pos 1 2 3 filename))) ]) ] |] ]

[<TestCaseSource(nameof testInputAst)>]
let testAst2Ir2Ast decl =
    let t: Intermediate =
        { Type = None
          Value = None
          Axiom = None
          TransitionSystem = None }

    let actual = convertToAst (convertToIntermediate decl t)

    Assert.AreEqual(decl, actual)


let testInputFiles: obj[] list =
    [
        [|"Samples/IrValueOrder.rsl"|]
        [|"Samples/AxiomGeneric.rsl"|]
        [|"Samples/TransitionSystem.rsl"|]
        [|"Samples/NamedTransitionRules.rsl"|]
        [|"Samples/NamedTransitionRules1.rsl"|]
        [|"Samples/NamedTransitionRules2.rsl"|]
        [|"Samples/NamedTransitionRules3.rsl"|]
        [|"Samples/NamedTransitionRules4.rsl"|]
        [|"Samples/NamedTransitionRules5.rsl"|]
        [|"Samples/NamedTransitionRules_unfolded.rsl"|]
    ]

[<TestCaseSource(nameof testInputFiles)>]
let testFileAstToIrToAst input =
    
    let spec, cls as astFromFile = 
        match testLexerAndParserFromFile input with
        | Some v -> v
        | None -> failwith ""
                 
    let ir = convertToIntermediate cls { Type = None; Value = None; Axiom = None; TransitionSystem = None }
    
    let astFromIr = Scheme(($"{fst spec}", snd spec), convertToAst ir)
    
    compareScheme(astFromFile, astFromIr)