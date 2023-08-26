module Transpiler.Transpiler

open Transpiler.RuleCollection.QuantificationRule
open Transpiler.RuleCollection.GenericAccessRule
open Transpiler.RuleCollection.GenericValueDefinitionRule
open Transpiler.RuleCollection.AxiomRule1
open Transpiler.RuleCollection.VariableRule
open Transpiler.RuleCollection.Cata
open Transpiler.RuleCollection.TransitionSystemRule1
open Transpiler.Helpers.Helpers
open Transpiler.Ast


let transpile2 ((specification, cls): Scheme) =
    let valueEnvironment = buildValueEnvironment cls

    let typeEnvironment = buildSymbolTable cls valueEnvironment

    (*
    1. X Quantified expression must be unfolded.
    2. X All generic access expression is replaced with its concrete equivalent.
    3. X All generic value definitions must be replaced with its explicit concrete equivalent.
    4. X All generic variable definitions must be replaced with its explicit concrete equivalent.
    5. X Axioms must be removed and used to make the value definitions explicit.
    6. X Explicit variable definitions must be made implicit by moving the information to the init constraint.
    7. X References to named transition rules must be replaced with the content of that rule
    *)


    let apply f (a, b, c) = f a b c

    let _, _, cls' =
        (typeEnvironment, valueEnvironment, cls)
        |> apply (valueExprCata unfoldQuantified)
        |> apply unfoldGenericValueDeclarations
        |> apply (valueExprCata unfoldGenericAccess)
        |> apply makeValueDeclarationExplicit
        |> apply makeVariableDeclarationImplicit
        |> apply unfoldNamedTransitionRules1

    Scheme(($"{fst specification}_unfolded", snd specification), cls')


let transpile ((specification, cls): Scheme) = transpile2 (specification, cls)
(*let valueEnvironment = buildValueEnvironment cls

    let typeEnvironment = buildSymbolTable cls valueEnvironment
    let valueTypeEnvironment = buildValueTypeTable cls

    let intermediate =
        convertToIntermediate
            cls
            { Type = None
              Value = None
              Axiom = None
              TransitionSystem = None
              LtlAssertion = None }

    let typeUnfolded =
        unfoldType typeEnvironment valueTypeEnvironment valueEnvironment intermediate

    let axiomsUnfolded =
        unfoldAxioms typeEnvironment valueTypeEnvironment valueEnvironment typeUnfolded

    let genericsTypeUnfolded =
        unfoldGenerics typeEnvironment valueTypeEnvironment valueEnvironment axiomsUnfolded

    let namedTransitionRulesUnfolded =
        unfoldNamedTransitionRules typeEnvironment valueTypeEnvironment valueEnvironment genericsTypeUnfolded


    let t = convertToAst namedTransitionRulesUnfolded
    Scheme(($"{fst specification}_unfolded", snd specification), t)*)
