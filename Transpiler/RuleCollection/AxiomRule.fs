/// <summary>
/// This is the IR rewriting associated with unfolding axioms.
/// </summary>
module Transpiler.RuleCollection.AxiomRule

open Transpiler.Intermediate
open Transpiler.RuleCollection.Helpers

/// <summary>
/// Go through all axioms in the axiom declaration and add the unfolded axioms to the value map and remove the
/// generic declared value.
/// </summary>
/// <param name="typeEnv"></param>
/// <param name="valueEnv"></param>
/// <param name="intermediate"></param>
let unfoldAxioms typeEnv valueEnv (intermediate: Intermediate) =
    let map =
        match intermediate.Value with
        | Some m -> m
        | None -> Map.empty

    let map =
        match intermediate with
        | { Axiom = axiomDecl } ->
            match axiomDecl with
            | None -> map
            | Some axioms -> List.foldBack (fun e a -> axiomFolder typeEnv valueEnv a Map.empty e) axioms map

    { intermediate with
        Value = Some(map)
        Axiom = None }
