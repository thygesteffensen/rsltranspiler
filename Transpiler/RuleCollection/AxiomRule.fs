/// <summary>
/// This is the IR rewriting associated with unfolding axioms.
/// </summary>
module Transpiler.RuleCollection.AxiomRule

open Transpiler

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
    (map: Map<string, ValueDeclaration>)
    (instances: Map<string, string>)
    (axiom: IrAxiomDeclaration)
    =
    match axiom with
    | IrQuantified(typings, valueExpr) ->
        instantiateTypings typeEnv valueEnv map instances valueExpr  typings
        
    | IrInfix(identifier, rhs) ->
        match identifier with
        | Simple s -> 
            let valueType = Map.find s valueEnv
            map.Add(s, ExplicitValue(s, valueType, rhs))
        | Generic(s, valueExpressions) -> 
            let valueType = Map.find s valueEnv
            
            let stringer ve =
                match ve with
                | ValueLiteral valueLiteral -> literalToString valueLiteral
                | VName s ->
                    match s with
                    | Simple s -> Map.find s instances
                    | Generic(s, valueExpressions) -> failwith "todo"
                | Quantified foo -> failwith "todo"

            let t = List.foldBack (fun e a -> $"_{stringer e}{a}") valueExpressions ""
            let tt = $"{s}{t}"
            let map' = map.Remove s
            
            map'.Add(tt, ExplicitValue(tt, valueType, rhs))

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
        let t =
            match typeExpr with
            | Literal _ -> failwith "todo"
            | TName s -> s
            | Product _ -> failwith "todo"
            | Set _ -> failwith "todo"
            | List _ -> failwith "todo"
            | Map _ -> failwith "todo"

        match Map.find t typeEnv with
        | Abstract -> failwith "todo"
        | Concrete _ -> failwith "todo"
        | Union l ->
            List.foldBack (fun v m -> instantiateTypings typeEnv valueEnv m (Map.add s v instances) valueExpr  ts) l map

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
        | { Type = _
            Value = _
            Axiom = axiomDecl } ->
            match axiomDecl with
            | None -> map
            | Some axioms ->
                List.foldBack (fun e a -> axiomFolder typeEnv valueEnv a Map.empty e) axioms map

    { intermediate with
        Value = Some(map)
        Axiom = None }
