module TranspilerTest.Parser.TestIr

open System.IO
open NUnit.Framework
open Transpiler
open Transpiler.Ast
open Transpiler.Reader
open TranspilerTest.Compare

open Transpiler.Helpers.Helpers


[<SetUp>]
let setup () = ()

let testInput: obj[] list =
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

[<TestCaseSource(nameof testInput)>]
let test input =
    
    let spec, cls as astFromFile = 
        match testLexerAndParserFromFile input with
        | Some v -> v
        | None -> failwith ""
                 
    let ir = convertToIntermediate cls { Type = None; Value = None; Axiom = None; TransitionSystem = None }
    
    let astFromIr = Scheme(($"{fst spec}", snd spec), convertToAst ir)
    
    use streamWriter1 = new StreamWriter("/home/thyge/Downloads/AstFromIr.txt", false)
    streamWriter1.Write(astFromIr)
    
    use streamWriter2 = new StreamWriter("/home/thyge/Downloads/AstFromFile.txt", false)
    streamWriter2.Write(astFromFile)
    
    compareScheme(astFromFile, astFromIr)
    