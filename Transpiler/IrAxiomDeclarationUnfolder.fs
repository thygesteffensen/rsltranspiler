module Transpiler.RuleCollection.Helpers

open Transpiler.Ast
open Transpiler.Helpers.Helpers
open Transpiler.Intermediate

/// <summary>
/// Convert a value expression to a string is possible
/// </summary>
/// <param name="ve"></param>
/// <param name="valueEnv"></param>
let valueExpressionToStringNew (ve: ValueExpression) (valueEnv: ValueEnvMap) =
    match ve with
    | ValueLiteral valueLiteral -> literalToString (fst valueLiteral)
    | VName s ->
        match s with
        | ASimple s -> 
            match Map.tryFind (fst s) valueEnv with
            | None -> fst s // TODO: Should the default just be the string assuming the type checker handles this?
            | Some value -> getValueLiteralString value
        | AGeneric _ -> failwith "todo"
    | ValueExpression.Quantified _ -> failwith "todo"
    | VPName _ -> failwith "todo"
    | Rule _ -> failwith "todo"
    | Infix _ -> failwith "todo"
    | VeList _ -> failwith "todo"
    | VArray _ -> failwith "todo"
    | LogicalNegation _ -> failwith "todo"

/// <summary>
/// We allow unfolding value expression until the
/// </summary>
/// <param name="typeEnv"></param>
/// <param name="valueTypeEnv"></param>
/// <param name="map"></param>
/// <param name="_valueEnv"></param>
/// <param name="instances"></param>
/// <param name="axiom"></param>
let rec axiomFolder typeEnv valueTypeEnv (map: ValueDecMap) valueEnv (axiom: IrAxiomDeclaration) =
    match axiom with
    | IrQuantified(typings, valueExpr) -> instantiateTypings typeEnv valueTypeEnv map valueEnv valueExpr typings

    | IrInfix(identifier, rhs) ->
        let findIndex (key: string) : int * string =
            let i, _ = Map.findKey (fun (_i, k) _v -> k = key) map
            (i, key)

        let rhs' = replaceNameWithValue valueEnv rhs
        match identifier with
        | ASimple(s, pos) ->
            let valueType = Map.find s valueTypeEnv
            map.Add(findIndex s, ExplicitValue(Identifier.ISimple(s, pos), valueType, rhs'))
        | AGeneric((s, pos), valueExpressions) ->
            let valueType = Map.find s valueTypeEnv

            let postfix =
                List.foldBack (fun e a -> $"_{valueExpressionToStringNew e valueEnv}{a}") valueExpressions ""

            let identifier = $"{s}{postfix}"
            let i, _k = findIndex s

            map.Add((i, identifier), ExplicitValue(ISimple(identifier, pos), valueType, rhs'))

/// <summary>
/// Iterate through each typing in the typing list and for each combination the value expression <see cref="valueExpr"/>
/// is unfolded and added to the value declaration map.
/// </summary>
/// <param name="typeEnv">Type environment</param>
/// <param name="valueTypeEnv">Value environment</param>
/// <param name="map">Value declaration map</param>
/// <param name="valueEnv">Value declaration map</param>
/// <param name="valueExpr">Value expression to be unfolded</param>
/// <param name="typing">List of typing for which a value expression is unfolded</param>
and instantiateTypings typeEnv valueTypeEnv map valueEnv valueExpr (typing: Typing list) =
    // TODO: This can be simplified using the new environments
    match typing with
    | [] ->
        // When we get here, all typings for this quantified expression is instantiated
        // and we are ready for continuing unfolded the quantified inner expression.
        axiomFolder typeEnv valueTypeEnv map valueEnv valueExpr // Inner for loop
    | SingleTyping(s, typeExpr) as _ :: ts ->
        match s with
        | ISimple(id, _pos) ->

            let t =
                match typeExpr with
                | Literal _ -> failwith "todo"
                | TName s -> s
                | Product _ -> failwith "todo"
                | Set _ -> failwith "todo"
                | List _ -> failwith "todo"
                | Map _ -> failwith "todo"
                | TArray _ -> failwith "todo"
                | Sub _ -> failwith "todo"

            match fst (Map.find (fst t) typeEnv) with
            | Abstract -> failwith "todo"
            | Concrete typeExpression ->
                match typeExpression with
                | Sub([ SingleTyping(ISimple(s0, _), typeExpression) ],
                      Infix(Infix(VName(ASimple(s, _)), GreaterThanOrEqual, ValueLiteral(VInt 0, _)),
                            LogicalAnd,
                            Infix(VName(ASimple(s1, _)), LessThan, ValueLiteral(VInt upperbound, _)))) ->
                    match typeExpression with
                    | TName("Nat", _) -> ()
                    | TName("Int", _) -> ()
                    | _ -> failwith ""

                    if not (s0 = s1 && s1 = s) then
                        failwith ""

                    // TODO: change to make use of pre computed value set
                    List.foldBack
                        (fun v m -> instantiateTypings typeEnv valueTypeEnv m (Map.add id v valueEnv)  valueExpr ts)
                        (List.map VInt [ 0..(upperbound-1) ])
                        map

                | _ -> failwith "Not supported"
            | Union l ->
                List.foldBack
                    (fun (e, _pos) m -> instantiateTypings typeEnv valueTypeEnv m (Map.add id (VText e) valueEnv) valueExpr ts)
                    l
                    map
        | IGeneric _ -> failwith "todo"
