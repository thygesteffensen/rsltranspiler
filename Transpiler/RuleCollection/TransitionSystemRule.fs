module Transpiler.RuleCollection.TransitionSystemRule

open Transpiler.Intermediate

let rec tr (ir: IrTransitionRules) (namedRules: NamedRuleMap) : IrTransitionRules =
    match ir with
    | Node(lhs, choice, rhs) -> Node(tr lhs namedRules, choice, tr rhs namedRules)
    | Leaf irTransitionRule ->
        match irTransitionRule with
        | Name s ->
            match Map.tryPick (fun (_i, k) e -> if k = s then Some e else None) namedRules with
            // match Map.tryFind s namedRules with
            | None -> failwith $"Named rule {s} is not defined"
            | Some value -> value
        | _ -> Leaf irTransitionRule


let trans (ir: IrTransitionSystem) : IrTransitionSystem =
    match ir.TransitionRule with
    | None -> ir
    | Some(irTransitionRules, irTransitionRulesMap) ->
        { ir with
            TransitionRule = (tr irTransitionRules irTransitionRulesMap, Map.empty) |> Some }


let unfoldNamedTransitionRules _typeEnv _valueEnv (intermediate: Intermediate) : Intermediate =
    match intermediate.TransitionSystem with
    | None -> intermediate
    | Some transitionSystem ->
        { intermediate with
            TransitionSystem = trans transitionSystem |> Some }
