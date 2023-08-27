module Transpiler.RuleCollection.Cata

open Transpiler.Ast
open Transpiler.Intermediate
open Transpiler.Helpers.Helpers

type valueExprFunc = TypeEnvMap -> ValueEnvMap -> ValueExpression -> ValueExpression

let rec flattenLogicalAndInfixValueExpr
    (valueExpr: ValueExpression)
    (acc: ValueExpression list)
    : ValueExpression list =
    match valueExpr with
    | Infix(lhs, LogicalAnd, rhs) -> flattenLogicalAndInfixValueExpr lhs acc |> flattenLogicalAndInfixValueExpr rhs
    | _ -> valueExpr :: acc


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

let typeDefFolder
    (func: valueExprFunc)
    (typeEnv: TypeEnvMap)
    (valueEnv: ValueEnvMap)
    (typeDef: Pos<Id> * TypeDefinition)
    (a: (Pos<Id> * TypeDefinition) list)
    : (Pos<Id> * TypeDefinition) list =
    match typeDef with
    | tuple, Concrete(Sub(tl, valueExpr)) ->
        let valueExpr' = func typeEnv valueEnv valueExpr
        (tuple, Concrete(Sub(tl, valueExpr')))
    | _ -> typeDef
    :: a

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
    | TypeDeclaration typeDecls ->
        List.foldBack (typeDefFolder func typeEnv valueEnv) typeDecls []
        |> TypeDeclaration
    | AxiomDeclaration valueExpressions ->
        // TODO: Consider if the is the right choice
        // Axioms are not "normal" value expressions, they are either equal infix with name or quantified this.
        // To avoid that lhs name is not replaced with its value, this is done:
        let t = 
            List.foldBack
                (fun e a ->
                    (match e with
                     | Infix(VName(ASimple _) as id, Equal, valueExpr) -> Infix(id, Equal, func typeEnv valueEnv valueExpr)
                     | _ -> func typeEnv valueEnv e)
                    :: a)
                valueExpressions
                []

        List.foldBack flattenLogicalAndInfixValueExpr t [] |> AxiomDeclaration
    | TransitionSystemDeclaration(idPos, transitionSystems) ->
        (idPos, List.foldBack (transitionSystemsFolder func typeEnv valueEnv) transitionSystems [])
        |> TransitionSystemDeclaration
    | LtlAssertionDeclaration tuples ->
        List.foldBack (ltlAssertionFolder func typeEnv valueEnv) tuples []
        |> LtlAssertionDeclaration
    :: a


let valueExprCata
    (func: valueExprFunc)
    (typeEnv: TypeEnvMap)
    (valueEnv: ValueEnvMap)
    (cls: Class)
    : TypeEnvMap * ValueEnvMap * Class =
    let cls' = List.foldBack (classFolder func typeEnv valueEnv) cls []
    // TODO: Optimise how the map is constructed instead of rebuilding from scratch!
    let valueEnv' = buildValueEnvironment cls'

    let typeEnvironment = buildSymbolTable cls' valueEnv'

    (typeEnvironment, valueEnv', cls')
