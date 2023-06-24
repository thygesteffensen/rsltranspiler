module Transpiler.RuleCollection.Helpers

open Transpiler.Ast
open Transpiler.Intermediate

/// <summary>
/// toString for value literal
/// </summary>
/// <param name="valueLiteral"></param>
let literalToString valueLiteral =
    match valueLiteral with
    | VUnit unit -> failwith "todo"
    | VBool b -> failwith "todo"
    | VInt i -> string i
    | VReal i -> string i
    | VChar c -> string c
    | VNat i -> string i
    | VText s -> s

/// <summary>
/// Convert a value expression to a string is possible
/// </summary>
/// <param name="ve"></param>
/// <param name="instances"></param>
let valueExpressionToString (ve: ValueExpression) (instances: Map<string, string>) =
    match ve with
    | ValueLiteral valueLiteral -> literalToString (fst valueLiteral)
    | VName s ->
        match s with
        | ASimple s ->
            match Map.tryFind (fst s) instances with
            | None -> fst s // TODO: Should the default just be the string assuming the type checker handles this?
            | Some value -> value
        | AGeneric(s, valueExpressions) -> failwith "todo"
    | ValueExpression.Quantified foo -> failwith "todo"
    | VPName accessor -> failwith "todo"
    | Rule tuple -> failwith "todo"
    | Infix foo -> failwith "todo"
    | VeList valueExpressions -> failwith "todo"

/// <summary>
/// We allow unfolding value expression until the
/// </summary>
/// <param name="typeEnv"></param>
/// <param name="valueEnv"></param>
/// <param name="map"></param>
/// <param name="instances"></param>
/// <param name="axiom"></param>
let rec axiomFolder
    typeEnv
    valueEnv
    (map: ValueDecMap)
    (instances: Map<string, string>)
    (axiom: IrAxiomDeclaration)
    =
    match axiom with
    | IrQuantified(typings, valueExpr) -> instantiateTypings typeEnv valueEnv map instances valueExpr typings

    | IrInfix(identifier, rhs) ->
        let findIndex (key: string) : int * string =
            let i, _ = Map.findKey (fun (_i, k) _v -> k = key) map
            (i, key)
        match identifier with
        | ASimple (s, pos) ->
            let valueType = Map.find s valueEnv
            map.Add(findIndex s, ExplicitValue(Identifier.ISimple (s, pos), valueType, rhs))
        | AGeneric((s, pos), valueExpressions) ->
            let valueType = Map.find s valueEnv

            let t = List.foldBack (fun e a -> $"_{valueExpressionToString e instances}{a}") valueExpressions ""
            let tt = $"{s}{t}"
            let i, _k as key = findIndex s
            // let map' = map.Remove key

            map.Add((i, tt), ExplicitValue(ISimple(tt, pos), valueType, rhs))

/// <summary>
/// Iterate through each typing in the typing list and for each combination the value expression <see cref="valueExpr"/>
/// is unfolded and added to the value declaration map.
/// </summary>
/// <param name="typeEnv">Type environment</param>
/// <param name="valueEnv">Value environment</param>
/// <param name="map">Value declaration map</param>
/// <param name="instances">Instances of generic variable to concrete typings or values</param>
/// <param name="valueExpr">Value expression to be unfolded</param>
/// <param name="typing">List of typing for which a value expression is unfolded</param>
and instantiateTypings typeEnv valueEnv map (instances: Map<string, string>) valueExpr (typing: Typing list) =
    match typing with
    | [] ->
        // When we get here, all typings for this quantified expression is instantiated
        // and we are ready for continuing unfolded the quantified inner expression.
        axiomFolder typeEnv valueEnv map instances valueExpr // Inner for loop
    | SingleTyping(s, typeExpr) :: ts ->
        match s with
        | ISimple (id, _pos) ->

            let t =
                match typeExpr with
                | Literal _ -> failwith "todo"
                | TName s -> s
                | Product _ -> failwith "todo"
                | Set _ -> failwith "todo"
                | List _ -> failwith "todo"
                | Map _ -> failwith "todo"

            match Map.find (fst t) typeEnv with
            | Abstract -> failwith "todo"
            | Concrete _ -> failwith "todo"
            | Union l ->
                List.foldBack
                    (fun (v, _) m -> instantiateTypings typeEnv valueEnv m (Map.add id v instances) valueExpr ts)
                    l
                    map
        | IGeneric _ -> failwith "todo"