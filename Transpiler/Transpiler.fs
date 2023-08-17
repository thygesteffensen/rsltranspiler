module Transpiler.Transpiler

open Transpiler.RuleCollection.AxiomRule
open Transpiler.RuleCollection.TypeRule
open Transpiler.RuleCollection.TransitionSystemRule
open Transpiler.Helpers.Helpers
open Transpiler.Ast

let transpile ((specification, cls): Scheme) =
    let valueEnvironment = buildValueEnvironment cls

    let typeEnvironment = buildSymbolTable cls valueEnvironment
    let valueTypeEnvironment = buildValueTypeTable cls

    let intermediate =
        convertToIntermediate
            cls
            { Type = None
              Value = None
              Axiom = None
              TransitionSystem = None }
    
    let typeUnfolded = unfoldType typeEnvironment valueTypeEnvironment valueEnvironment intermediate

    let axiomsUnfolded = unfoldAxioms typeEnvironment valueTypeEnvironment valueEnvironment typeUnfolded

    let genericsTypeUnfolded =
        unfoldGenerics typeEnvironment valueTypeEnvironment valueEnvironment axiomsUnfolded

    let namedTransitionRulesUnfolded =
        unfoldNamedTransitionRules typeEnvironment valueTypeEnvironment valueEnvironment genericsTypeUnfolded

    let t = convertToAst namedTransitionRulesUnfolded
    Scheme(($"{fst specification}_unfolded", snd specification), t)
