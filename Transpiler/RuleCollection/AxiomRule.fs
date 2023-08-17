/// <summary>
/// This is the IR rewriting associated with unfolding axioms.
/// </summary>
module Transpiler.RuleCollection.AxiomRule

open Transpiler.Ast
open Transpiler.Intermediate
open Transpiler.RuleCollection.Helpers
open Transpiler.Helpers.Helpers


let _unfoldAxioms typeEnv valueTypeEnv valueDeclarations valueEnv axioms =
        
    let valueDeclarationsAxiomsUnfolded = List.foldBack (fun e a -> axiomFolder typeEnv valueTypeEnv a valueEnv e) axioms valueDeclarations
    // All Axioms should be unfolded, i.e. all Typing (IGeneric should be replaced with it's thingy
    // How do we best do this?
    // 1. Produce a list and when each generic is computed, the element is removed from the list, yielding a list for
    //  non assigned constants
    // 2. When the list is done, go through each generic and verify it have been instantiated in all instances
    let missing =
        Map.foldBack
            (fun (i, _k) e a ->
                match e with
                | Typing(SingleTyping(IGeneric((s, _pos), typings), _)) ->
                    iterateTypings typeEnv s typings (fun e a ->
                        match Map.tryFind (i, e) valueDeclarationsAxiomsUnfolded with
                            | None -> e :: a
                            | Some _ -> a
                            ) []

                | _ -> a // Only target non assigned generic variables
            )
            valueDeclarations // Initial value declarations
            [] // List of missing unfolded identifiers

    if missing.Length > 0 then
        let sep = ", "
        failwith $"The following unfolded generics are missing: {String.concat sep missing}"

    let cleaned =
        Map.foldBack
            (fun k e a ->
                match e with
                | Typing(SingleTyping(IGeneric _, _)) -> Map.remove k a
                | _ -> a)
            valueDeclarationsAxiomsUnfolded
            valueDeclarationsAxiomsUnfolded

    Some(cleaned)

/// <summary>
/// Go through all axioms in the axiom declaration and add the unfolded axioms to the value map and remove the
/// generic declared value.
/// </summary>
/// <param name="typeEnv"></param>
/// <param name="valueTypeEnv"></param>
/// <param name="valueEnv"></param>
/// <param name="intermediate"></param>
let unfoldAxioms typeEnv valueTypeEnv valueEnv (intermediate: Intermediate) =
    match intermediate with
    | { Axiom = axiomDecl } ->
        match axiomDecl with
        | None -> intermediate
        | Some axioms ->
            let valueDeclarations =
                match intermediate.Value with
                | Some m -> m
                | None -> Map.empty
                
            { intermediate with
                Value = _unfoldAxioms typeEnv valueTypeEnv valueDeclarations valueEnv axioms
                Axiom = None }
