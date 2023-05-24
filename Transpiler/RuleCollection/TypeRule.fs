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

let mapFolder typeEnv valueEnv (s: Map<string, ValueDeclaration>) k v =
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

let rec unfoldValueExpression typeEnv valueEnv (v: ValueExpression) =
    match v with
    | ValueLiteral _ as vl -> vl 
    | VName accessor -> failwith "todo"
    | VPName accessor -> failwith "todo"
    | Rule _ as r -> r
    | Quantified foo -> failwith "todo"
    | Infix(lhs, infixOp, rhs) ->
        let lhs' = unfoldValueExpression typeEnv valueEnv lhs
        let rhs' = unfoldValueExpression typeEnv valueEnv rhs
        Infix(lhs', infixOp, rhs')

/// <summary>
/// Named Rule folder
/// 1. Takes a IR Named rule in
/// 2. Output unfolded IR Named Rule
///
/// Quantified expression over the choice must be unfolded.
/// </summary>
/// <param name="typeEnv"></param>
/// <param name="valueEnv"></param>
/// <param name="s">Rule Map</param>
/// <param name="k">Named Rule name</param>
/// <param name="v">Intermediate Representation Transition Rule to be unfolded</param>
let rec namedTransitionRuleFolder typeEnv valueEnv (s: Map<string, IrTr>) k (v: IrTr) =
    let unfoldedRule = namedTransitionRuleFolder typeEnv valueEnv Map.empty k v
    Map.add k s unfoldedRule

/// <summary>
/// This is folding over the NamedRules map array and should unfold them
///
/// Guarded should be guarded?
/// Quantified should become a chain?
/// </summary>
/// <param name="typeEnv"></param>
/// <param name="valueEnv"></param>
/// <param name="instances">Instances</param>
/// <param name="k"></param>
/// <param name="v"></param>
let rec namedTransitionRuleFolder typeEnv valueEnv (instances: Map<string, string>) k (v: IrTr) =
    match v with
    | Single irRule ->
        match irRule with
        | Guarded _ as g -> v
        | Named _ as n -> v
        | Quan(NonDeterministic, typings, irRule) ->
            // Choice decides how each irRule should be linked together
            // each typing needs to be instantiated and for each instance, the irRule unfolded
            // irRule is the expression that must be unfolded
            typeInstancer typeEnv valueEnv instances k irRule typings None 
                
        | Quan(Choice.Deterministic, _, _) ->
            failwith "Transition Rules cannot be quantified over the deterministic choice operator"
                
            
    | Chain(irRule, choice, irTr) -> failwith "todo"

and typeInstancer typeEnv valueEnv (instances: Map<string, string>) k v (typings: Typing list) acc =
    match typings with
    | [] ->
        // No more typings to instantiate, proceed to unfolding
        match acc with
        | None -> namedTransitionRuleFolder typeEnv valueEnv instances k v
        | Some ->
            Chain(
                namedTransitionRuleFolder typeEnv valueEnv instances k v,
                choice,
                acc)
        
    | SingleTyping(id, typeExpr)::ts ->
        match id with
        | ISimple (id, _pos) ->

            let t =
                match typeExpr with
                | TName s -> s
                | _ -> failwith "todo"

            match Map.find (fst t) typeEnv with
            | Abstract -> failwith "todo"
            | Concrete _ -> failwith "todo"
            | Union l ->
                List.foldBack
                    (fun (v', _) map -> typeInstancer typeEnv valueEnv (Map.add id v' instances) map k v ts)
                    l
                    ruleMap
        | IGeneric _ -> failwith "todo"

/// <summary>
/// Unfold all generics including:
///     * a
///     * b
/// </summary>
/// <param name="typeEnv"></param>
/// <param name="valueEnv"></param>
/// <param name="intermediate"></param>
let unfoldGenerics typeEnv valueEnv (intermediate: Intermediate) =
    // q: Why use map as state and input?
    // a: The we don't have to add un-processed items to the new state, since they are already there
    
    let unfoldedValueMap =
        match intermediate.Value with
        | Some m -> Map.fold (mapFolder typeEnv valueEnv) m m |> Some
        | None -> None

    let ts =
        match intermediate.TransitionSystem with
        | None -> None
        | Some ts ->
            let variable =
                match ts.Variable with
                | None -> None
                | Some value ->
                    Map.fold (mapFolder typeEnv valueEnv) value value |> Some
            
            let tr =
                match ts.TransitionRule with
                | None -> None
                | Some value ->
                    Some({ Rule = value.Rule
                           NamedRules = Map.fold (namedTransitionRuleFolder typeEnv valueEnv) value.NamedRules value.NamedRules })
            
            Some({ ts with Variable = variable; TransitionRule = tr })    

    { intermediate with Value = unfoldedValueMap; TransitionSystem = ts }