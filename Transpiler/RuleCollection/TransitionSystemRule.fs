module Transpiler.RuleCollection.TransitionSystemRule
(*

open Transpiler

let rec unfold typeEnv valueEnv ruleEnv (rule: IrTr) =
    // For now we only allowed named transition rules on the highest level, i.e. they cannot appear inside a quantified
    // expression. The only places it can exist is in the first chain of infix operators using choice as separator.
    match rule with
    // TODO: DO
    | Single irRule -> failwith "todo"
    | Chain(irRule, choice, irTr) -> failwith "todo"
    (*| Infix(lhs, op, rhs) when op = InfixOp.Deterministic || op = InfixOp.NonDeterministic ->
        // This part is not tail recursive
        let lhs' = unfold typeEnv valueEnv ruleEnv lhs
        let rhs' = unfold typeEnv valueEnv ruleEnv rhs
        Infix(lhs', op, rhs')
    | Rule (r, _) ->
        Map.find r ruleEnv
    | _ -> rule // We have reached the allowed cut off*)


/// <summary>
/// Unfold Named Transition Rules
/// </summary>
/// <param name="typeEnv"></param>
/// <param name="valueEnv"></param>
/// <param name="intermediate"></param>
let unfoldNamedTransitionRules typeEnv valueEnv (intermediate: Intermediate) =

    let tt (ee: IrTransitionSystem) =
        let namedRules = ee.TransitionRule.Value.NamedRules
        let valueExpr = ee.TransitionRule.Value.Rule

        match namedRules.Count with
        | 0 -> ee
        | _ ->
            { ee with
                TransitionRule =
                    Some(
                        { NamedRules = namedRules
                          Rule = unfold typeEnv valueEnv namedRules valueExpr }
                    ) }

    match intermediate.TransitionSystem with
    | None -> intermediate
    | Some irTransitionSystem ->
        { intermediate with
            TransitionSystem = Some(tt irTransitionSystem) }
            *)
