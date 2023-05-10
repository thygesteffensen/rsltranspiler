/// <summary>
/// This is the IR rewriting associated with unfolding axioms.
/// </summary>
module Transpiler.RuleCollection.AxiomRule

open Transpiler

let valueExpressionToString (valueExpr: ValueExpression) =
    match valueExpr with
    | ValueLiteral valueLiteral -> failwith "todo"
    | VName s -> s
    | GenericName foo -> failwith "todo"
    | Equivalence foo -> failwith "todo"
    | Quantified foo -> failwith "todo"

let rec unfoldValueExpr typeEnv valueEnv (valueExpr: ValueExpression) =
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
    | Quantified foo -> failwith "todo"

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

let axiomFolder typeEnv valueEnv (map: Map<string, ValueDeclaration>) (axiom: ValueExpression) =
    match axiom with
    | Quantified(quantifier, typings, valueExpression) -> failwith "todo"
    | Equivalence(rhs, lhs) -> // Here
        
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
            | Some(AxiomDeclaration axioms) -> List.foldBack (fun e a -> a) axioms map
            | Some(TransitionSystemDeclaration _) -> failwith "todo"
            | None -> Map.empty 

    { intermediate with
        Value = Some(map)
        Axiom = None }
