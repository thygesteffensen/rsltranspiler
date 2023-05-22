module Transpiler.RuleCollection.TypeRule

open Transpiler

/// <summary>
/// Given a typing list, this will generate a list of all variants of the typing, prefixed with '_' and seperated by '_' .
/// Thus, the typings must be finite, otherwise it will continue forever.
///
/// Given
/// <code>
///     type
///         T1 == t1 | t2,
///         T2 = {| i: Int :- i >=0 /\ i < 3 |}
/// </code>
/// this would generate:
/// <code>
///     [ _t1_0
///       _t1_1
///       _t1_2
///       _t2_0
///       _t2_1
///       _t2_3 ]
/// </code>
///
/// </summary>
/// <param name="typingList">The typing list for which the post fixes are generated</param>
/// <param name="typeEnv">Type environment</param>
let buildTypePostfixStrings typeEnv valueEnv typingList =
    
    // Matrix
    let first :: second =
        List.foldBack
            (fun (SingleTyping(_, typeExpr)) acc ->
                match typeExpr with
                | TName n ->
                    match Map.find (fst n) typeEnv with
                    | Union l -> (List.foldBack (fun (e, _) bb -> e :: bb) l []) :: acc)
            typingList
            []

    let rec buildPostfix (list: string list list) (acc: string list) =
        match list with
        | [] -> List.foldBack (fun e acc -> $"_{e}" :: acc) acc []
        | x :: xs ->
            buildPostfix xs (List.foldBack (fun (p1, p2) acc1 -> $"{p2}_{p1}" :: acc1) (List.allPairs x acc) [])

    buildPostfix second first


let unfoldTypings typeEnv valueEnv (intermediate: Intermediate) =
    let map =
        match intermediate.Value with
        | Some m -> m
        | None -> Map.empty

    let mapFolder (s: Map<string, ValueDeclaration>) k v =
        match v with
        | GenericValue(ISimple(id, pos), typingList, typeExpression) ->
            let postfix = buildTypePostfixStrings typeEnv valueEnv typingList

            let s' = s.Remove k

            List.foldBack
                (fun e (acc: Map<string, ValueDeclaration>) ->
                    acc.Add($"{id}{e}", Typing(SingleTyping(ISimple($"{id}{e}", pos), typeExpression))))
                postfix
                s'
        | _ -> s

    // q: Why use map as state and input?
    // a: The we don't have to add un-processed items to the new state, since they are already there
    let map' = Map.fold mapFolder map map

    { intermediate with Value = Some(map') }