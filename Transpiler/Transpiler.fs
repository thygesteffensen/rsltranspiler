module Transpiler.Transpiler

open Transpiler.RuleCollection.AxiomRule
open Transpiler.RuleCollection.TypeRule
open Transpiler.RuleCollection.TransitionSystemRule
open Transpiler.Helpers.Helpers
open Transpiler.Ast

let transpile ((specification, cls): Scheme) =
    let typeEnvironment = buildSymbolTable cls
    let valueEnvironment = buildValueTable cls

    let intermediate =
        convertToIntermediate
            cls
            { Type = None
              Value = None
              Axiom = None
              TransitionSystem = None }

    let axiomsUnfolded = unfoldAxioms typeEnvironment valueEnvironment intermediate

    let genericsTypeUnfolded =
        unfoldGenerics typeEnvironment valueEnvironment axiomsUnfolded
        
    let namedTransitionRulesUnfolded =
        unfoldNamedTransitionRules typeEnvironment valueEnvironment genericsTypeUnfolded

    let t = convertToAst namedTransitionRulesUnfolded
    Scheme(($"{fst specification}_unfolded", snd specification), t)
