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

let unfoldAccessor1
    _typeEnv
    _valueTypeEnv
    (valueEnv: ValueEnvMap)
    (accessor: Accessor)
    (f: Accessor -> ValueExpression)
    : ValueExpression =
    match accessor with
    | ASimple(id, position) ->
        match Map.tryFind id valueEnv with
        | None -> f accessor
        | Some value -> ValueLiteral(value, position)
    | AGeneric((id, pos), valueExprs) ->
        let postfix =
            List.foldBack (fun e a -> (valueExpressionToStringNew e valueEnv) + a) valueExprs ""
        f (ASimple(id + "_" + postfix, pos))

let unfoldAccessor
    _typeEnv
    _valueTypeEnv
    (valueEnv: ValueEnvMap)
    (accessor: Accessor)
    (f: Accessor -> ValueExpression)
    : ValueExpression =
    match accessor with
    | ASimple(id, position) ->
        match Map.tryFind id valueEnv with
        | None -> f accessor
        | Some value -> ValueLiteral(value, position)
    | AGeneric((id, pos), valueExprs) ->
        let postfix =
            List.foldBack (fun e a -> (valueExpressionToStringNew e valueEnv) + a) valueExprs ""
            
        f (ASimple(id + "_" + postfix, pos))

let rec unfoldValueExpression
    (typeEnv: TypeEnvMap)
    valueTypeEnv
    (valueEnv: ValueEnvMap)
    (v: ValueExpression)
    : ValueExpression =

    match v with
    | ValueExpression.Quantified((quantifier, _pos), typings, valueExpression) ->
        let delimiter =
            match quantifier with
            | All -> LogicalAnd
            | Exists -> LogicalAnd
            | ExactlyOne -> failwith "ExactlyOne is not supported"
            | Quantifier.Deterministic -> InfixOp.Deterministic
            | Quantifier.NonDeterministic -> InfixOp.NonDeterministic


        let rec typingFolder
            (typings: Typing list)
            (valueEnv': ValueEnvMap)
            (acc: ValueExpression list)
            : ValueExpression list =
            match typings with
            | SingleTyping(ISimple(id, _pos), TName(tName, _pos1)) :: ts ->
                match Map.tryFind tName typeEnv with
                | None -> failwith $"Could not find {tName} in type environment"
                | Some(_typeDef, instances) ->
                    List.foldBack (fun instance a -> typingFolder ts (Map.add id instance valueEnv') a) instances acc
            | [] ->
                unfoldValueExpression typeEnv valueTypeEnv valueEnv' valueExpression
                :: acc
            | _ -> failwith "Given typing not supported"

        let l = typingFolder typings valueEnv [] // Yields a list of the value expressions
        let ll = List.reduce (fun e a -> Infix(e, delimiter, a)) l // List reduced to a single infix expression

        ll
    | Infix(lhs, infixOp, rhs) ->
        let lhs' = unfoldValueExpression typeEnv valueTypeEnv valueEnv lhs
        let rhs' = unfoldValueExpression typeEnv valueTypeEnv valueEnv rhs
        Infix(lhs', infixOp, rhs')
    | VeList l ->
        List.foldBack (fun e a -> (unfoldValueExpression typeEnv valueTypeEnv valueEnv e) :: a) l []
        |> VeList
    | VName accessor -> unfoldAccessor typeEnv valueTypeEnv valueEnv accessor VName
    | VPName accessor -> unfoldAccessor typeEnv valueTypeEnv valueEnv accessor VPName 
    | ValueLiteral tuple -> v
    | Rule(s, position) -> v
    | VArray valueExpressions ->
        List.foldBack (fun e a -> unfoldValueExpression typeEnv valueTypeEnv valueEnv e :: a) valueExpressions [] |> VArray
    | LogicalNegation(valueExpression, position) ->
        LogicalNegation(unfoldValueExpression typeEnv valueTypeEnv valueEnv valueExpression, position)
    | Prefix(tuple, valueExpression) ->
        Prefix(tuple, unfoldValueExpression typeEnv valueTypeEnv valueEnv valueExpression)


/// <summary>
///
/// </summary>
/// <param name="typeEnv"></param>
/// <param name="valueTypeEnv"></param>
/// <param name="valueEnv"></param>
/// <param name="ir"></param>
/// <param name="acc"></param>
let rec irAxiomDecUnfold
    typeEnv
    valueTypeEnv
    valueEnv
    (ir: IrAxiomDeclaration)
    acc
    : IrAxiomDeclaration list =
    match ir with
    | IrQuantified(typings, ir') -> instantiateTypings typeEnv valueTypeEnv valueEnv typings ir' acc
    | IrInfix(accessor, valueExpression) ->
        match accessor with
        | ASimple _ -> ir :: acc // There is no need to unfold a simple accessor
        | AGeneric((id, pos), valueExpressions) ->
            // TODO: Consider that the value expression can hold generic accessors
            let stringifyId =
                List.foldBack (fun e a -> a + "_" + (valueExpressionToStringNew e valueEnv)) valueExpressions id

            IrInfix(ASimple(stringifyId, pos), valueExpression) :: acc

and instantiateTypings
    (typeEnv: TypeEnvMap)
    valueTypeEnv
    valueEnv
    (typings: Typing list)
    (ir: IrAxiomDeclaration)
    acc
    : IrAxiomDeclaration list =
    match typings with
    | [] -> irAxiomDecUnfold typeEnv valueTypeEnv valueEnv ir acc
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
            | (l: ValueLiteral list) ->
                List.foldBack
                    (fun e a -> instantiateTypings typeEnv valueTypeEnv (Map.add s e valueEnv) ts ir a)
                    l
                    acc


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
        Node(irTransitionRulesUnfold typeEnv valueTypeEnv valueEnv lhs, choice, irTransitionRulesUnfold typeEnv valueTypeEnv valueEnv rhs)
    | Leaf irTransitionRule -> irTransitionRuleUnfold typeEnv valueTypeEnv valueEnv irTransitionRule

and irTransitionRuleUnfold
    typeEnv
    valueTypeEnv
    valueEnv
    (rule: IrTransitionRule)
    : IrTransitionRules =
    match rule with
    | Guarded(valueExpression, tuples) ->
        let t =
            unfoldValueExpression typeEnv valueTypeEnv valueEnv valueExpression

        Guarded(
            t,
            List.foldBack
                (fun (a, e) acc ->
                    let t =
                        match unfoldAccessor typeEnv valueTypeEnv valueEnv a VName with
                        | VName v -> v
                        | VPName v -> v
                    (t,
                     (unfoldValueExpression typeEnv valueTypeEnv valueEnv e))
                    :: acc)
                tuples
                []
        )
        |> Leaf
    | Name _ -> Leaf rule
    | Quantified(Deterministic, _, _) -> failwith "Not possible"
    | Quantified(NonDeterministic, typings, irTransitionRule) ->
        instantiateTypings1 typeEnv valueTypeEnv valueEnv typings irTransitionRule

and instantiateTypings1
    typeEnv
    valueTypeEnv
    valueEnv
    (typings: Typing list)
    (rule: IrTransitionRule)
    : IrTransitionRules =
    match typings with
    | [] -> irTransitionRuleUnfold typeEnv valueTypeEnv valueEnv rule
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
                            instantiateTypings1 typeEnv valueTypeEnv (Map.add s e valueEnv) ts rule
                        ))
                    valueSet
                    (instantiateTypings1 typeEnv valueTypeEnv (Map.add s v valueEnv) ts rule)

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
                List.foldBack (fun e a -> irAxiomDecUnfold typeEnv valueTypeEnv Map.empty e a) value []
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

    { intermediate with
        Value = unfoldedValueMap
        TransitionSystem = transitionSystemFolder typeEnv valueTypeEnv valueEnv intermediate.TransitionSystem }



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
