module Transpiler.RuleCollection.TypeRule

open Transpiler.Ast
open Transpiler.Intermediate
open Transpiler.RuleCollection.Helpers
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


    | GenericValue(ISimple _, _, _) ->
        // TODO: This part is obsolete
        failwith "Is it absolute?"
    | _ -> s


let unfoldAccessor _typeEnv _valueTypeEnv (instances: Map<string, string>) (accessor: Accessor) : Accessor =
    match accessor with
    | ASimple _ -> accessor
    | AGeneric((id, pos), valueExprs) ->
        let postfix =
            List.foldBack (fun e a -> (valueExpressionToString e instances) + a) valueExprs ""

        ASimple(id + "_" + postfix, pos)

let rec unfoldValueExpression typeEnv valueTypeEnv (instances: Map<string, string>) (v: ValueExpression) =
    match v with
    | ValueExpression.Quantified _ -> failwith "todo"
    | Infix(lhs, infixOp, rhs) ->
        let lhs' = unfoldValueExpression typeEnv valueTypeEnv instances lhs
        let rhs' = unfoldValueExpression typeEnv valueTypeEnv instances rhs
        Infix(lhs', infixOp, rhs')
    | VeList l ->
        List.foldBack (fun e a -> (unfoldValueExpression typeEnv valueTypeEnv instances e) :: a) l []
        |> VeList
    | VName accessor -> unfoldAccessor typeEnv valueTypeEnv instances accessor |> VName
    | VPName accessor -> unfoldAccessor typeEnv valueTypeEnv instances accessor |> VPName
    | _ -> v


/// <summary>
///
/// </summary>
/// <param name="typeEnv"></param>
/// <param name="valueTypeEnv"></param>
/// <param name="instances"></param>
/// <param name="ir"></param>
/// <param name="acc"></param>
let rec irAxiomDecUnfold
    typeEnv
    valueTypeEnv
    (instances: Map<string, string>)
    (ir: IrAxiomDeclaration)
    acc
    : IrAxiomDeclaration list =
    match ir with
    | IrQuantified(typings, ir') -> instantiateTypings typeEnv valueTypeEnv instances typings ir' acc
    | IrInfix(accessor, valueExpression) ->
        match accessor with
        | ASimple _ -> ir :: acc // There is no need to unfold a simple accessor
        | AGeneric((id, pos), valueExpressions) ->
            // TODO: Consider that the value expression can hold generic accessors
            let stringifyId =
                List.foldBack (fun e a -> a + "_" + (valueExpressionToString e instances)) valueExpressions id

            IrInfix(ASimple(stringifyId, pos), valueExpression) :: acc

and instantiateTypings
    (typeEnv: TypeEnvMap)
    valueTypeEnv
    (instances: Map<string, string>)
    (typings: Typing list)
    (ir: IrAxiomDeclaration)
    acc
    : IrAxiomDeclaration list =
    match typings with
    | [] -> irAxiomDecUnfold typeEnv valueTypeEnv instances ir acc
    | SingleTyping(identifier, typeExpr) :: ts ->
        match identifier with
        | IGeneric _ -> failwith "todo"
        | ISimple(s, _pos) ->
            let t =
                match typeExpr with
                | TName s -> s
                | _ -> failwith "todo"

            match snd (Map.find (fst t) typeEnv) with
            | [] -> failwith "todo"
            // TODO: Replace values after unfolding
            | l ->
                List.foldBack
                    (fun e a -> instantiateTypings typeEnv valueTypeEnv (Map.add s e instances) ts ir a)
                    l
                    acc


/// <summary>
/// Unfold Transition Rule
/// * Unfold Quantified rules
/// </summary>
/// <param name="typeEnv"></param>
/// <param name="valueTypeEnv"></param>
/// <param name="rule"></param>
let rec irTransitionRulesUnfold typeEnv valueTypeEnv (rule: IrTransitionRules) : IrTransitionRules =
    match rule with
    | Node(lhs, choice, rhs) ->
        Node(irTransitionRulesUnfold typeEnv valueTypeEnv lhs, choice, irTransitionRulesUnfold typeEnv valueTypeEnv rhs)
    | Leaf irTransitionRule -> irTransitionRuleUnfold typeEnv valueTypeEnv Map.empty irTransitionRule

and irTransitionRuleUnfold
    typeEnv
    valueTypeEnv
    (instances: Map<string, string>)
    (rule: IrTransitionRule)
    : IrTransitionRules =
    match rule with
    | Guarded(valueExpression, tuples) ->
        let t = unfoldValueExpression typeEnv valueTypeEnv instances valueExpression
        Guarded(
            t,
            List.foldBack
                (fun (a, e) acc ->
                    ((unfoldAccessor typeEnv valueTypeEnv instances a),
                     (unfoldValueExpression typeEnv valueTypeEnv instances e))
                    :: acc)
                tuples
                []
        )
        |> Leaf
    | Name _ -> Leaf rule
    | Quantified(Deterministic, _, _) -> failwith "Not possible"
    | Quantified(NonDeterministic, typings, irTransitionRule) ->
        instantiateTypings1 typeEnv valueTypeEnv instances typings irTransitionRule

and instantiateTypings1
    typeEnv
    valueTypeEnv
    (instances: Map<string, string>)
    (typings: Typing list)
    (rule: IrTransitionRule)
    : IrTransitionRules =
    match typings with
    | [] -> irTransitionRuleUnfold typeEnv valueTypeEnv instances rule
    | SingleTyping(id, typeExpr) :: ts ->
        match id with
        | IGeneric _ -> failwith "todo"
        | ISimple(s, _pos) ->
            let t =
                match typeExpr with
                | TName s -> s
                | _ -> failwith "todo"

            match (Map.find (fst t) typeEnv) with
            | _, [] -> failwith $"Cannot unfold infinite types ({fst t})"
            | Abstract, _ -> failwith "todo"
            // Union and Sub expression value set are computed when building the symbol table
            // and this set can be iterated to know each instance of a types values
            | Union _, v :: valueSet
            | Concrete _, v :: valueSet ->
                List.foldBack
                    (fun e a ->
                        Node(
                            a,
                            NonDeterministic,
                            instantiateTypings1 typeEnv valueTypeEnv (Map.add s e instances) ts rule
                        ))
                    valueSet
                    (instantiateTypings1 typeEnv valueTypeEnv (Map.add s v instances) ts rule)

/// <summary>
/// Unfold transition rule
/// </summary>
/// <param name="typeEnv"></param>
/// <param name="valueTypeEnv"></param>
/// <param name="rule"></param>
/// <param name="namedRules"></param>
let transitionRuleUnfold
    typeEnv
    valueTypeEnv
    (rule: IrTransitionRules)
    (namedRules: NamedRuleMap)
    : IrTransitionRules * NamedRuleMap =

    (irTransitionRulesUnfold typeEnv valueTypeEnv rule,
     Map.foldBack (fun k e a -> Map.add k (irTransitionRulesUnfold typeEnv valueTypeEnv e) a) namedRules namedRules)


/// <summary>
/// Unfold generic type in a transition system.
/// * Variable declarations must be unfolded, just as value declarations
/// * Init Constraint must be unfolded, similar to axiom
/// * Transition System must be unfolded w.r.t. generic types and quantified rules
/// </summary>
/// <param name="typeEnv"></param>
/// <param name="valueTypeEnv"></param>
/// <param name="transitionSystemOption"></param>
let transitionSystemFolder
    typeEnv
    valueTypeEnv
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
                List.foldBack (fun e a -> irAxiomDecUnfold typeEnv valueTypeEnv Map.empty e a) value []
                |> Some

        let unfoldedTransitionRule =
            match value.TransitionRule with
            | None -> None
            | Some(irTransitionRules, irTransitionRulesMap) ->
                transitionRuleUnfold typeEnv valueTypeEnv irTransitionRules irTransitionRulesMap
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
/// <param name="intermediate"></param>
let unfoldGenerics typeEnv valueTypeEnv (intermediate: Intermediate) =
    // q: Why use map as state and input?
    // a: The we don't have to add un-processed items to the new state, since they are already there

    let unfoldedValueMap =
        match intermediate.Value with
        | Some m -> Map.foldBack (mapFolder typeEnv valueTypeEnv) m m |> Some
        | None -> None

    { intermediate with
        Value = unfoldedValueMap
        TransitionSystem = transitionSystemFolder typeEnv valueTypeEnv intermediate.TransitionSystem }

let rec replaceNameWithValue valueEnv (valueExpr: ValueExpression) : ValueExpression =
    match valueExpr with
    | VName(ASimple(name, pos)) ->
        match Map.tryFind name valueEnv with
        | None -> valueExpr
        | Some value -> (ValueLiteral (value, pos))
    | ValueExpression.Quantified(tuple, typings, valueExpression) ->
        ValueExpression.Quantified(tuple, typings, replaceNameWithValue valueEnv valueExpression)
    | Infix(lhs, op, rhs) -> Infix(replaceNameWithValue valueEnv lhs, op, replaceNameWithValue valueEnv rhs)
    | VeList valueExpressions ->
        List.foldBack (fun e a -> (replaceNameWithValue valueEnv e) :: a) valueExpressions []
        |> VeList
    | VArray valueExpressions ->
        List.foldBack (fun e a -> (replaceNameWithValue valueEnv e) :: a) valueExpressions []
        |> VArray
    | LogicalNegation(valueExpression, position) ->
        LogicalNegation(replaceNameWithValue valueEnv valueExpression, position)
    | _ -> valueExpr

/// <summary>
/// Unfold type declaration, sub type expression are unfolded
/// </summary>
/// <param name="_typeEnv"></param>
/// <param name="_valueTypeEnv"></param>
/// <param name="valueEnv"></param>
/// <param name="intermediate"></param>
let unfoldType _typeEnv _valueTypeEnv valueEnv (intermediate: Intermediate) : Intermediate =

    let unfoldedType =
        match intermediate.Type with
        | None -> None
        | Some(tuples) ->
            List.foldBack
                (fun (k, v as e) a ->
                    match v with
                    | Concrete(Sub(typings, valueExpression)) ->
                        (k, Concrete(Sub(typings, replaceNameWithValue valueEnv valueExpression)))
                    | Abstract -> e
                    | Union _ -> e
                    | Concrete _ -> e
                    :: a)
                tuples
                []
            |> Some


    { intermediate with
        Type = unfoldedType }
