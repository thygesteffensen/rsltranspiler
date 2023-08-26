module Transpiler.RuleCollection.Cata

open Transpiler.Ast
open Transpiler.Intermediate

type valueExprFunc = TypeEnvMap -> ValueEnvMap -> ValueExpression -> ValueExpression

let valueDeclarationFolder
    (func: valueExprFunc)
    (typeEnv: TypeEnvMap)
    (valueEnv: ValueEnvMap)
    (e: ValueDeclaration)
    (a: ValueDeclaration list)
    : ValueDeclaration list =
    match e with
    | ExplicitValue(identifier, typeExpression, valueExpression) ->
        ExplicitValue(identifier, typeExpression, func typeEnv valueEnv valueExpression)
    | Typing _ -> e // Does not contain any value expression
    :: a

let transitionSystemsFolder
    (func: valueExprFunc)
    (typeEnv: TypeEnvMap)
    (valueEnv: ValueEnvMap)
    (e: TransitionSystem)
    (a: TransitionSystem list)
    : TransitionSystem list =
    match e with
    | Variable valueDeclarations ->
        List.foldBack (valueDeclarationFolder func typeEnv valueEnv) valueDeclarations []
        |> Variable
    | InitConstraint valueExpr -> func typeEnv valueEnv valueExpr |> InitConstraint
    | TransitionRule(valueExpr, tuples) ->
        // TODO: Consider if this is the best way to do it...
        let names, exprs = List.unzip tuples

        let t = List.foldBack (fun e a -> func typeEnv valueEnv e :: a) exprs []

        TransitionRule(func typeEnv valueEnv valueExpr, List.zip names t)
    :: a

let ltlAssertionFolder
    (func: valueExprFunc)
    (typeEnv: TypeEnvMap)
    (valueEnv: ValueEnvMap)
    ((name, ts, valueExpr): LtlAssertion)
    (a: LtlAssertion list)
    : LtlAssertion list =
    (name, ts, func typeEnv valueEnv valueExpr) :: a

let classFolder
    (func: valueExprFunc)
    (typeEnv: TypeEnvMap)
    (valueEnv: ValueEnvMap)
    (e: Declaration)
    (a: Class)
    : Class =
    match e with
    | Value valueDeclarations ->
        List.foldBack (valueDeclarationFolder func typeEnv valueEnv) valueDeclarations []
        |> Value
    | TypeDeclaration _ -> e // Does not contain any value expression
    | AxiomDeclaration valueExpressions ->
        List.foldBack (fun e a -> (func typeEnv valueEnv e) :: a) valueExpressions []
        |> AxiomDeclaration
    | TransitionSystemDeclaration(idPos, transitionSystems) ->
        (idPos, List.foldBack (transitionSystemsFolder func typeEnv valueEnv) transitionSystems [])
        |> TransitionSystemDeclaration
    | LtlAssertionDeclaration tuples ->
        List.foldBack (ltlAssertionFolder func typeEnv valueEnv) tuples []
        |> LtlAssertionDeclaration
    :: a


let valueExprCata
    (typeEnv: TypeEnvMap)
    (valueEnv: ValueEnvMap)
    (func: valueExprFunc)
    (cls: Class)
    : Class =
    List.foldBack (classFolder func typeEnv valueEnv) cls []
