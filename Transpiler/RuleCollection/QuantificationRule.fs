module Transpiler.RuleCollection.QuantificationRule

open Transpiler.Ast
open Transpiler.Intermediate
open Transpiler.Helpers.Helpers


let rec unfoldQuantified (typeEnv: TypeEnvMap) (valueEnv: ValueEnvMap) (valueExpr: ValueExpression) : ValueExpression =

    match valueExpr with
    // TODO: References to in axioms are replaced with the value....
    | VName accessor ->
        match accessor with
        | ASimple(id, position) ->
            // valueExpr
            match Map.tryFind id valueEnv with
            | None -> valueExpr
            | Some value -> ValueLiteral(value, position)
        | AGeneric((id, pos), valueExprs) ->
            let postfix =
                List.foldBack (fun e a -> "_" + (valueExpressionToString e valueEnv) + a) valueExprs ""

            VName(ASimple(id + postfix, pos))
    | VPName accessor ->
        match accessor with
        | ASimple _ -> valueExpr
        | AGeneric((id, pos), valueExprs) ->
            let postfix =
                List.foldBack (fun e a -> (valueExpressionToString e valueEnv) + a) valueExprs ""

            VPName(ASimple(id + "_" + postfix, pos))
    | ValueLiteral _
    | Rule _ -> valueExpr
    | Infix(lhs, infixOp, rhs) ->
        let lhs' = unfoldQuantified typeEnv valueEnv lhs
        let rhs' = unfoldQuantified typeEnv valueEnv rhs
        Infix(lhs', infixOp, rhs')
    | VeList l ->
        List.foldBack (fun e a -> unfoldQuantified typeEnv valueEnv e :: a) l []
        |> VeList
    | VArray l ->
        List.foldBack (fun e a -> unfoldQuantified typeEnv valueEnv e :: a) l []
        |> VArray
    | LogicalNegation(valueExpression, position) ->
        LogicalNegation(unfoldQuantified typeEnv valueEnv valueExpression, position)
    | Prefix(tmoPos, valueExpression) -> Prefix(tmoPos, unfoldQuantified typeEnv valueEnv valueExpression)
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
                    List.fold (fun a instance -> typingFolder ts (Map.add id instance valueEnv') a) acc instances
            | [] -> unfoldQuantified typeEnv valueEnv' valueExpression :: acc
            | _ -> failwith "Given typing not supported"

        let l = typingFolder typings valueEnv [] // Yields a list of the value expressions
        let ll = List.reduce (fun e a -> Infix(a, delimiter, e)) l // List reduced to a single infix expression

        ll
