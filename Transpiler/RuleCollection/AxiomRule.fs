/// <summary>
/// This is the IR rewriting associated with unfolding axioms.
/// </summary>
module Transpiler.RuleCollection.AxiomRule

open Transpiler

let flattenGenericName (valueExpr: ValueExpression) =
    let literalToString valueLiteral =
        match valueLiteral with
        | VUnit unit -> failwith "todo"
        | VBool b -> failwith "todo"
        | VInt i -> string i
        | VReal i -> string i
        | VChar c -> string c
        | VNat i -> string i
        | VText s -> s
        
    match valueExpr with
    | ValueLiteral i -> literalToString i
    | VName n -> n
    | GenericName(s, valueExpressions) ->
        let stringer ve =
            match ve with
            | ValueLiteral valueLiteral -> literalToString valueLiteral 
            | VName s -> s
            | GenericName foo -> failwith "todo"
            | Equivalence foo -> failwith "todo"
            | Quantified foo -> failwith "todo"
            
        let t = List.foldBack (fun e a -> $"{stringer e}_{a}") valueExpressions ""
        $"{s}_{t}"
    | Equivalence _ -> failwith "Equivalence expressions are not allowed here"
    | Quantified _ -> failwith "Quantified expressions are not allowed here"


/// <summary>
/// We allow unfolding value expression until the
/// </summary>
/// <param name="typeEnv"></param>
/// <param name="valueEnv"></param>
/// <param name="map"></param>
/// <param name="instances"></param>
/// <param name="axiom"></param>
let rec axiomFolder typeEnv valueEnv (map: Map<string, ValueDeclaration>) (instances: Map<string, string>) (axiom: ValueExpression) =
    match axiom with
    | Quantified(quantifier, typings, valueExpr) ->
        // 1. Create instance lookup
        
        // Instances is a map from string to string
        // key: Is the tempt bounded variable
        // value: Is the current value of the key, used to unfold
        
        // Iterate over typings in the typing list and increment each typing once - x nested for loop
        // 
        let rec instantiateTypings (typing: Typing list) (instances: Map<string,string>) valueExpr' =
            match typing with
            | [] ->
                // When we get here, all typings for this quantified expression is instantiated
                // and we are ready for continuing unfolded the quantified inner expression.
                axiomFolder typeEnv valueEnv map instances valueExpr' // Inner for loop
            | SingleTyping(s, typeExpr)::ts -> // n'th for looooooop
                let t  =
                    match typeExpr with
                    | Literal typeLiteral -> failwith "todo"
                    | TName s -> s
                    | Product typeExpressions -> failwith "todo"
                    | Set typeExpression -> failwith "todo"
                    | List typeExpression -> failwith "todo"
                    | Map foo -> failwith "todo"
                
                match Map.find t typeEnv with
                | Abstract -> failwith "todo"
                | Concrete typeExpression -> failwith "todo"
                | Union l ->
                    // It is folding the list of variants, for each variant it recurse into itself,
                    // populating the instance to instances and reached a point where all typings are
                    // instantiated and then the valueExpr can be unfolded.
                    List.foldBack (fun v m -> instantiateTypings ts (Map.add s v instances) valueExpr') l map
                     
        instantiateTypings typings instances valueExpr
    | Equivalence(rhs, lhs) ->
        let flattened = flattenGenericName rhs
        match rhs with
        | VName s ->
            let valueType = Map.find s valueEnv
            map.Add(s, ExplicitValue(s, valueType, lhs))
            
        | GenericName(s, valueExpressions) ->
            // 1. Create instance lookup 
            // For each instance add s of instance and flattened to map
            let valueType = Map.find s valueEnv
            let t = "" // This need to be the string name, which I have already done somewhere
            map.Add(flattened, ExplicitValue(flattened, valueType, lhs))
        | _ -> failwith "Rhs can only be a value or variable name"
        
    | _ -> failwith "Axiom can only be a Quantified expression or an Equivalence"

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
            | Some(Value _) -> failwith "todo"
            | Some(TypeDeclaration _) -> failwith "todo"
            | Some(AxiomDeclaration axioms) -> List.foldBack (fun e a -> axiomFolder typeEnv valueEnv a Map.empty e) axioms map
            | Some(TransitionSystemDeclaration _) -> failwith "todo"
            | None -> Map.empty 

    { intermediate with
        Value = Some(map)
        Axiom = None }
