module Transpiler.RuleCollection.TypeRule


open Transpiler.Ast
open Transpiler.Intermediate
open Transpiler.RuleCollection.Helpers
open Transpiler.Auxiliary



// TODO: Cleanup and create doc
let mapFolder typeEnv valueEnv (s: ValueDecMap) (i, k as key) v =
    match v with
    | Typing(SingleTyping(IGeneric((id, position), typings), expression)) ->
        let postfixes = buildTypePostfixStrings typeEnv valueEnv typings
        
        let s' = s.Remove key

        List.foldBack
            (fun e (acc: ValueDecMap) ->
                acc.Add((i, $"{id}{e}"), Typing(SingleTyping(ISimple($"{id}{e}", position), expression))))
            postfixes
            s'

    | GenericValue(ISimple(id, pos), typingList, typeExpression) ->
        // TODO: This part is obsolete\
        failwith "Is it absolute?"
        let postfix = buildTypePostfixStrings typeEnv valueEnv typingList

        let s' = s.Remove key

        List.foldBack
            (fun e (acc: ValueDecMap) ->
                acc.Add((i, $"{id}{e}"), Typing(SingleTyping(ISimple($"{id}{e}", pos), typeExpression))))
            postfix
            s'
    | _ -> s


let unfoldAccessor _typeEnv _valueEnv (instances: Map<string, string>) (accessor: Accessor) : Accessor =
    match accessor with
    | ASimple _ -> accessor
    | AGeneric((id, pos), valueExprs) ->
        let postfix =
            List.foldBack (fun e a -> (valueExpressionToString e instances) + a) valueExprs ""

        ASimple(id + "_" + postfix, pos)

let rec unfoldValueExpression typeEnv valueEnv (instances: Map<string, string>) (v: ValueExpression) =
    match v with
    | ValueExpression.Quantified _ -> failwith "todo"
    | Infix(lhs, infixOp, rhs) ->
        let lhs' = unfoldValueExpression typeEnv valueEnv instances lhs
        let rhs' = unfoldValueExpression typeEnv valueEnv instances rhs
        Infix(lhs', infixOp, rhs')
    | VeList l ->
        List.foldBack (fun e a -> (unfoldValueExpression typeEnv valueEnv instances e) :: a) l []
        |> VeList
    | VName accessor -> unfoldAccessor typeEnv valueEnv instances accessor |> VName
    | VPName accessor -> unfoldAccessor typeEnv valueEnv instances accessor |> VPName
    | _ -> v


/// <summary>
///
/// </summary>
/// <param name="typeEnv"></param>
/// <param name="valueEnv"></param>
/// <param name="instances"></param>
/// <param name="ir"></param>
/// <param name="acc"></param>
let rec irAxiomDecUnfold
    typeEnv
    valueEnv
    (instances: Map<string, string>)
    (ir: IrAxiomDeclaration)
    acc
    : IrAxiomDeclaration list =
    match ir with
    | IrQuantified(typings, ir') -> instantiateTypings typeEnv valueEnv instances typings ir' acc
    | IrInfix(accessor, valueExpression) ->
        match accessor with
        | ASimple _ -> ir :: acc // There is no need to unfold a simple accessor
        | AGeneric((id, pos), valueExpressions) ->
            // TODO: Consider that the value expression can hold generic accessors
            let stringifyId =
                List.foldBack (fun e a -> a + "_" + (valueExpressionToString e instances)) valueExpressions id

            IrInfix(ASimple(stringifyId, pos), valueExpression) :: acc

and instantiateTypings
    typeEnv
    valueEnv
    (instances: Map<string, string>)
    (typings: Typing list)
    (ir: IrAxiomDeclaration)
    acc
    : IrAxiomDeclaration list =
    match typings with
    | [] -> irAxiomDecUnfold typeEnv valueEnv instances ir acc
    | SingleTyping(identifier, typeExpr) :: ts ->
        match identifier with
        | IGeneric _ -> failwith "todo"
        | ISimple(s, _pos) ->
            let t =
                match typeExpr with
                | TName s -> s
                | _ -> failwith "todo"

            match Map.find (fst t) typeEnv with
            | Abstract -> failwith "todo"
            | Concrete _ -> failwith "todo"
            | Union l ->
                List.foldBack
                    (fun (e, _) a -> instantiateTypings typeEnv valueEnv (Map.add s e instances) ts ir a)
                    l
                    acc


/// <summary>
/// Unfold Transition Rule
/// * Unfold Quantified rules
/// </summary>
/// <param name="typeEnv"></param>
/// <param name="valueEnv"></param>
/// <param name="rule"></param>
let rec irTransitionRulesUnfold typeEnv valueEnv (rule: IrTransitionRules) : IrTransitionRules =
    match rule with
    | Node(lhs, choice, rhs) ->
        Node(irTransitionRulesUnfold typeEnv valueEnv lhs, choice, irTransitionRulesUnfold typeEnv valueEnv rhs)
    | Leaf irTransitionRule -> irTransitionRuleUnfold typeEnv valueEnv Map.empty irTransitionRule

and irTransitionRuleUnfold
    typeEnv
    valueEnv
    (instances: Map<string, string>)
    (rule: IrTransitionRule)
    : IrTransitionRules =
    match rule with
    | Guarded(valueExpression, tuples) ->
        Guarded(
            unfoldValueExpression typeEnv valueEnv instances valueExpression,
            List.foldBack
                (fun (a, e) acc ->
                    ((unfoldAccessor typeEnv valueEnv instances a),
                     (unfoldValueExpression typeEnv valueEnv instances e))
                    :: acc)
                tuples
                []
        )
        |> Leaf
    | Name _ -> Leaf rule
    | Quantified(Deterministic, _, _) -> failwith "Not possible"
    | Quantified(NonDeterministic, typings, irTransitionRule) ->
        instantiateTypings1 typeEnv valueEnv instances typings irTransitionRule

and instantiateTypings1
    typeEnv
    valueEnv
    (instances: Map<string, string>)
    (typings: Typing list)
    (rule: IrTransitionRule)
    : IrTransitionRules =
    match typings with
    | [] -> irTransitionRuleUnfold typeEnv valueEnv instances rule
    | SingleTyping(id, typeExpr) :: ts ->
        match id with
        | IGeneric _ -> failwith "todo"
        | ISimple(s, _pos) ->
            let t =
                match typeExpr with
                | TName s -> s
                | _ -> failwith "todo"

            match Map.find (fst t) typeEnv with
            | Abstract -> failwith "todo"
            | Concrete _ -> failwith "todo"
            | Union [] -> failwith "todo"
            | Union ((l, _)::ls) ->
                List.foldBack
                    (fun (e, _) a ->
                        Node(a, NonDeterministic, instantiateTypings1 typeEnv valueEnv (Map.add s e instances) ts rule))
                    ls
                    (instantiateTypings1 typeEnv valueEnv (Map.add s l instances) ts rule)


/// <summary>
/// Unfold transition rule
/// </summary>
/// <param name="typeEnv"></param>
/// <param name="valueEnv"></param>
/// <param name="rule"></param>
/// <param name="namedRules"></param>
let transitionRuleUnfold
    typeEnv
    valueEnv
    (rule: IrTransitionRules)
    (namedRules: NamedRuleMap)
    : IrTransitionRules * NamedRuleMap =

    (irTransitionRulesUnfold typeEnv valueEnv rule,
     Map.foldBack (fun k e a -> Map.add k (irTransitionRulesUnfold typeEnv valueEnv e) a) namedRules namedRules)


/// <summary>
/// Unfold generic type in a transition system.
/// * Variable declarations must be unfolded, just as value declarations
/// * Init Constraint must be unfolded, similar to axiom
/// * Transition System must be unfolded w.r.t. generic types and quantified rules
/// </summary>
/// <param name="typeEnv"></param>
/// <param name="valueEnv"></param>
/// <param name="transitionSystemOption"></param>
let transitionSystemFolder
    typeEnv
    valueEnv
    (transitionSystemOption: Option<IrTransitionSystem>)
    : Option<IrTransitionSystem> =
    match transitionSystemOption with
    | None -> None
    | Some value ->
        let unfoldedVariables =
            match value.Variable with
            | None -> None
            | Some value -> Map.fold (mapFolder typeEnv valueEnv) value value |> Some

        let unfoldedInitConstraint =
            match value.InitConstraint with
            | None -> None
            | Some value ->
                None
                List.foldBack (fun e a -> irAxiomDecUnfold typeEnv valueEnv Map.empty e a) value []
                |> Some

        let unfoldedTransitionRule =
            match value.TransitionRule with
            | None -> None
            | Some(irTransitionRules, irTransitionRulesMap) ->
                transitionRuleUnfold typeEnv valueEnv irTransitionRules irTransitionRulesMap
                |> Some

        Some(
            { Name = value.Name
              Variable = unfoldedVariables
              InitConstraint = unfoldedInitConstraint
              TransitionRule = unfoldedTransitionRule }
        )

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

    { intermediate with
        Value = unfoldedValueMap
        TransitionSystem = transitionSystemFolder typeEnv valueEnv intermediate.TransitionSystem }
