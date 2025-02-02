module Transpiler.RuleCollection.TypeRule

open Transpiler.Ast
open Transpiler.Intermediate
open Transpiler.Helpers.Helpers

/// <summary>
/// Unfold Generic typings in ValueDecMap
/// </summary>
/// <param name="typeEnv"></param>
/// <param name="_valueTypeEnv"></param>
/// <param name="s"></param>
/// <param name="key"></param>
/// <param name="v"></param>
let mapFolder typeEnv _valueTypeEnv (i, _k as key) v (s: ValueDecMap) =
    match v with
    | Typing(SingleTyping(IGeneric((id, position), typings), expression)) ->
        let s' = s.Remove key

        iterateTypings
            typeEnv
            id
            typings
            (fun e (acc: ValueDecMap) -> acc.Add((i, e), Typing(SingleTyping(ISimple(e, position), expression))))
            s'
    | _ -> s

/// <summary>
///
/// </summary>
/// <param name="typeEnv"></param>
/// <param name="valueTypeEnv"></param>
/// <param name="valueEnv"></param>
/// <param name="ir"></param>
/// <param name="acc"></param>
let rec irAxiomDecUnfold typeEnv valueTypeEnv valueEnv acc (ir: IrAxiomDeclaration) : IrAxiomDeclaration list =
    match ir with
    | IrQuantified(typings, ir') ->
        genericInstantiateTypings typeEnv valueTypeEnv valueEnv typings acc ir' irAxiomDecUnfold
    // instantiateTypings typeEnv valueTypeEnv valueEnv typings ir' acc
    | IrInfix(accessor, valueExpression) ->
        match accessor with
        | ASimple _ -> ir :: acc // There is no need to unfold a simple accessor
        | AGeneric((id, pos), valueExpressions) ->
            let stringifyId =
                List.foldBack (fun e a -> a + "_" + (valueExpressionToString e valueEnv)) valueExpressions id

            IrInfix(ASimple(stringifyId, pos), valueExpression) :: acc


/// <summary>
/// Unfold Transition Rule
/// * Unfold Quantified rules
/// </summary>
/// <param name="typeEnv"></param>
/// <param name="valueTypeEnv"></param>
/// <param name="valueEnv"></param>
/// <param name="rule"></param>
let rec irTransitionRulesUnfold typeEnv valueTypeEnv valueEnv (rule: IrTransitionRules) : IrTransitionRules =
    match rule with
    | Node(lhs, choice, rhs) ->
        Node(
            irTransitionRulesUnfold typeEnv valueTypeEnv valueEnv lhs,
            choice,
            irTransitionRulesUnfold typeEnv valueTypeEnv valueEnv rhs
        )
    | Leaf irTransitionRule -> irTransitionRuleUnfold typeEnv valueTypeEnv valueEnv irTransitionRule

and irTransitionRuleUnfold typeEnv valueTypeEnv valueEnv (rule: IrTransitionRule) : IrTransitionRules =
    match rule with
    | Guarded(guard, effects) ->
        let unfoldedGuard = unfoldValueExpression typeEnv valueTypeEnv valueEnv guard

        Guarded(
            unfoldedGuard,
            List.foldBack
                (fun (id', effect) acc ->
                    (unfoldAccessor typeEnv valueTypeEnv valueEnv id' id (fun _ -> failwith ""),
                     unfoldValueExpression typeEnv valueTypeEnv valueEnv effect)
                    :: acc)
                effects
                []
        )
        |> Leaf
    | Name _ -> Leaf rule
    | Quantified(choice, typings, irTransitionRule) ->
        let list =
            genericInstantiateTypings typeEnv valueTypeEnv valueEnv typings [] irTransitionRule (fun te vte ve acc e ->
                irTransitionRuleUnfold te vte ve e :: acc)

        List.reduce (fun e a -> Node(e, choice, a)) list


/// <summary>
/// Unfold transition rule
/// </summary>
/// <param name="typeEnv"></param>
/// <param name="valueTypeEnv"></param>
/// <param name="valueEnv"></param>
/// <param name="rule"></param>
/// <param name="namedRules"></param>
let transitionRuleUnfold
    typeEnv
    valueTypeEnv
    valueEnv
    (rule: IrTransitionRules)
    (namedRules: NamedRuleMap)
    : IrTransitionRules * NamedRuleMap =

    (irTransitionRulesUnfold typeEnv valueTypeEnv valueEnv rule,
     Map.foldBack
         (fun k e a -> Map.add k (irTransitionRulesUnfold typeEnv valueTypeEnv valueEnv e) a)
         namedRules
         namedRules)


/// <summary>
/// Unfold generic type in a transition system.
/// * Variable declarations must be unfolded, just as value declarations
/// * Init Constraint must be unfolded, similar to axiom
/// * Transition System must be unfolded w.r.t. generic types and quantified rules
/// </summary>
/// <param name="typeEnv"></param>
/// <param name="valueTypeEnv"></param>
/// <param name="valueEnv"></param>
/// <param name="transitionSystemOption"></param>
let transitionSystemFolder
    typeEnv
    valueTypeEnv
    valueEnv
    (transitionSystemOption: Option<IrTransitionSystem>)
    : Option<IrTransitionSystem> =
    match transitionSystemOption with
    | None -> None
    | Some value ->
        let unfoldedVariables =
            match value.Variable with
            | None -> None
            | Some value -> Map.foldBack (mapFolder typeEnv valueTypeEnv) value value |> Some

        let unfoldedInitConstraint =
            match value.InitConstraint with
            | None -> None
            | Some value ->
                List.foldBack (fun e a -> irAxiomDecUnfold typeEnv valueTypeEnv Map.empty a e) value []
                |> Some

        let unfoldedTransitionRule =
            match value.TransitionRule with
            | None -> None
            | Some(irTransitionRules, irTransitionRulesMap) ->
                transitionRuleUnfold typeEnv valueTypeEnv valueEnv irTransitionRules irTransitionRulesMap
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
/// <param name="valueTypeEnv"></param>
/// <param name="valueEnv"></param>
/// <param name="intermediate"></param>
let unfoldGenerics typeEnv valueTypeEnv valueEnv (intermediate: Intermediate) =
    // q: Why use map as state and input?
    // a: The we don't have to add un-processed items to the new state, since they are already there

    let unfoldedValueMap =
        match intermediate.Value with
        | Some m -> Map.foldBack (mapFolder typeEnv valueTypeEnv) m m |> Some
        | None -> None
        
    let unfoldedLtl =
        match intermediate.LtlAssertion with
        | Some m -> List.foldBack (fun (name, ts, e) a -> (name, ts, (unfoldValueExpression typeEnv valueTypeEnv valueEnv e)) :: a) m [] |> Some
        | None -> None

    { intermediate with
        Value = unfoldedValueMap
        TransitionSystem = transitionSystemFolder typeEnv valueTypeEnv valueEnv intermediate.TransitionSystem
        LtlAssertion = unfoldedLtl }



/// <summary>
/// Unfold type declaration, sub type expression are unfolded
/// </summary>
/// <param name="typeEnv"></param>
/// <param name="valueTypeEnv"></param>
/// <param name="valueEnv"></param>
/// <param name="intermediate"></param>
let unfoldType typeEnv valueTypeEnv valueEnv (intermediate: Intermediate) : Intermediate =

    let unfoldedType =
        match intermediate.Type with
        | None -> None
        | Some(tuples) ->
            List.foldBack
                (fun (k, v as e) a ->
                    match v with
                    | Concrete(Sub(typings, valueExpression)) ->
                        (k, Concrete(Sub(typings, unfoldValueExpression typeEnv valueTypeEnv valueEnv valueExpression)))
                    | Abstract -> e
                    | Union _ -> e
                    | Concrete _ -> e
                    :: a)
                tuples
                []
            |> Some


    { intermediate with
        Type = unfoldedType }
