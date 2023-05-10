module Transpiler.Transpiler

open Transpiler.RuleCollection.AxiomRule
open Transpiler.RuleCollection.TypeRule
open Transpiler.Helpers



let transpile ((specification, cls): Scheme) =
    let typeEnvironment = buildSymbolTable cls
    let valueEnvironment = buildValueTable cls

    let intermediate =
        convertToIntermediate
            cls
            { Type = None
              Value = None
              Axiom = None }

    let axiomsUnfolded = unfoldAxioms typeEnvironment valueEnvironment intermediate

    let genericsTypeUnfolded =
        unfoldTypings typeEnvironment valueEnvironment axiomsUnfolded

    let t = convertToAst genericsTypeUnfolded []
    Scheme($"{specification}_unfolded", t)
