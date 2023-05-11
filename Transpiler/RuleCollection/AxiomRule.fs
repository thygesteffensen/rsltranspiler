/// <summary>
/// This is the IR rewriting associated with unfolding axioms.
/// </summary>
module Transpiler.RuleCollection.AxiomRule

open Transpiler

(*let valueExpressionToString (valueExpr: ValueExpression) =
    match valueExpr with
    | ValueLiteral valueLiteral -> failwith "todo"
    | VName s -> s
    | GenericName foo -> failwith "todo"
    | Equivalence foo -> failwith "todo"
    | Quantified foo -> failwith "todo"*)

(*let rec unfoldValueExpr typeEnv valueEnv (valueExpr: ValueExpression) =
    match valueExpr with
    | ValueLiteral valueLiteral -> failwith "todo"
    | VName s -> failwith "todo"
    | GenericName(id, valueExpressions) -> //failwith "todo" // Awesome, this is my stop
        // Every value expression evaluates to a value, which I have to look up
        // The easiest case, is that each value expr is a VName, which is in the instances, otherwise give up for now
        VName(
            id
            + List.foldBack (fun v s -> (valueExpressionToString v) + s) valueExpressions ""
        ) // And the resr
    | Equivalence(rhs, lhs) -> failwith "todo" //Equivalence(unfoldValueExpr instances rhs, unfoldValueExpr instances lhs)
    | Quantified foo -> failwith "todo"*)

(*
let unfoldValueExpression typeEnv valueEnv =
    function
    | Quantified(_, tl, ve) ->
        // Move to own thing
        // map from local identifier to current value (t1, t2, 1, 2 etc based on typing)
        let possibilities typeEnv typing =
            match typing with
            | TName n ->
                match Map.find n typeEnv with
                | Union l -> (List.foldBack (fun e bb -> e :: bb) l [])
                | _ -> failwith "Typing not supported for unfolding"
            | _ -> failwith "Typing not supported for unfolding"

        match ve with
        | Equivalence(GenericName(name, _), value_expr) ->
            let valueType = Map.find name valueEnv
            map <- map.Remove name

        let rec flatten typeEnv valueEnv (instances: Map<string, string>) tl ve' acc =
            match tl with
            | SingleTyping(i, typeExpr) :: ts ->
                let possibilities = possibilities typeEnv typeExpr

                List.foldBack (fun e a -> flatten typeEnv valueEnv (instances.Add(i, e)) ts ve' a) possibilities acc

            | [] -> Map.add "" (unfoldValueExpr instances ve') acc // Convert all generic accessors to the typed in, using instances and add them to the map

        ()
    | _ -> failwith "Not supported in axiom unfolding."
    *)

let flattenGenericName (valueExpr: ValueExpression) =
    match valueExpr with
    | ValueLiteral _ as vl -> vl
    | VName _ as n -> n
    | GenericName(s, valueExpressions) ->
        
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
                match Map.find s typeEnv with
                | Abstract -> failwith "todo"
                | Concrete typeExpression -> failwith "todo"
                | Union l ->
                    // It is folding the list of variants, for each variant it recurse into itself,
                    // populating the instance to instances and reached a point where all typings are
                    // instantiated and then the valueExpr can be unfolded.
                    List.foldBack (fun v m -> instantiateTypings ts (Map.add s v instances) valueExpr') l map
                     
        instantiateTypings typings instances valueExpr
    | Equivalence(rhs, lhs) ->
        let flattened = flattenGenericName lhs
        match rhs with
        | VName s ->
            let valueType = Map.find s valueEnv
            map.Add(s, ExplicitValue(s, valueType, flattened))
            
        | GenericName(s, valueExpressions) ->
            // 1. Create instance lookup 
            // For each instance add s of instance and flattened to map
            let valueType = Map.find s valueEnv
            let t = "" // This need to be the string name, which I have already done somewhere
            map.Add(t, ExplicitValue(t, valueType, flattened))
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
            | Some(AxiomDeclaration axioms) -> List.foldBack (fun e a -> a) axioms map // axiomFolder should be used here
            | Some(TransitionSystemDeclaration _) -> failwith "todo"
            | None -> Map.empty 

    { intermediate with
        Value = Some(map)
        Axiom = None }
