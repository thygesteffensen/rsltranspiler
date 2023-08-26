module Transpiler.RuleCollection.GenericAccessRule

open Transpiler.Ast
open Transpiler.Intermediate
open Transpiler.Helpers.Helpers

let rec unfoldGenericAccess
    (typeEnv: TypeEnvMap)
    (valueEnv: ValueEnvMap)
    (valueExpr: ValueExpression)
    : ValueExpression =
    let unfoldAccessor (valueEnv: ValueEnvMap) (accessor: Accessor) : Accessor =
        match accessor with
        | ASimple _ -> accessor
        | AGeneric((id, pos), valueExprs) ->
            let postfix =
                List.foldBack (fun e a -> (valueExpressionToString e valueEnv) + a) valueExprs ""

            ASimple(id + "_" + postfix, pos)

    match valueExpr with
    | ValueLiteral _
    | Rule _ -> valueExpr
    | VName accessor -> unfoldAccessor valueEnv accessor |> VName
    | VPName accessor -> unfoldAccessor valueEnv accessor |> VPName
    | Infix(lhs, infixOp, rhs) ->
        let lhs' = unfoldGenericAccess typeEnv valueEnv lhs
        let rhs' = unfoldGenericAccess typeEnv valueEnv rhs
        Infix(lhs', infixOp, rhs')
    | VeList l ->
        List.foldBack (fun e a -> unfoldGenericAccess typeEnv valueEnv e :: a) l []
        |> VeList
    | VArray l ->
        List.foldBack (fun e a -> unfoldGenericAccess typeEnv valueEnv e :: a) l []
        |> VArray
    | LogicalNegation(valueExpression, position) ->
        LogicalNegation(unfoldGenericAccess typeEnv valueEnv valueExpression, position)
    | Prefix(tmoPos, valueExpression) -> Prefix(tmoPos, unfoldGenericAccess typeEnv valueEnv valueExpression)
    | ValueExpression.Quantified((_, pos), _, _) ->
        // TODO: Use monad?
        failWithLine pos "Quantified Expressions must be unfolded before all generic accessors can be replaced."