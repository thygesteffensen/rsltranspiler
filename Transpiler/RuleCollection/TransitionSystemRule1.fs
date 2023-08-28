module Transpiler.RuleCollection.TransitionSystemRule1

open Transpiler.Ast
open Transpiler.Auxiliary
open Transpiler.Helpers

type RuleMapType = Map<Id, ValueExpression>

let rec exprWalker (valueExpr: ValueExpression) (ruleMap: RuleMapType) : ValueExpression =
    match valueExpr with
    | ValueLiteral _ -> valueExpr
    | VName(AGeneric((name, pos), _)) -> failWithLine pos $"Generic name \"{name}\" is not allowed at this stage"
    | VName(ASimple(name, _pos)) ->
        // If name cannot be found, it might be a variable?
        match Map.tryFind name ruleMap with
        | None -> valueExpr
        | Some value -> value
    | VPName _ ->
        // Named rules cannot be referenced as a prime accessor
        valueExpr
    | Rule(name, pos) ->
        match Map.tryFind name ruleMap with
        | None -> failWithLine pos $"Rule \"{name}\" not found..."
        | Some value -> value
    | ValueExpression.Quantified((_, pos), _, _) ->
        failWithLine pos "Quantified expressions are not allowed at this stage."
    | Infix(valueExpr1, infixOp, valueExpr2) ->
        Infix(exprWalker valueExpr1 ruleMap, infixOp, exprWalker valueExpr2 ruleMap)
    | VeList valueExprs -> List.foldBack (fun e a -> exprWalker e ruleMap :: a) valueExprs [] |> VeList
    | VArray valueExprs -> List.foldBack (fun e a -> exprWalker e ruleMap :: a) valueExprs [] |> VArray
    | LogicalNegation(valueExpr, pos) -> LogicalNegation(exprWalker valueExpr ruleMap, pos)
    | Prefix(idPos, valueExpr) -> Prefix(idPos, exprWalker valueExpr ruleMap)
    | Flat(infixOp, valueExprs) ->
        Flat(infixOp, List.foldBack (fun e a -> exprWalker e ruleMap :: a) valueExprs [])

let mapBuilder (((name, _pos), valueExpr): Pos<Id> * ValueExpression) (acc: RuleMapType) : RuleMapType =
    acc.Add(name, valueExpr)

let transitionSystemFolder (transitionSystem: TransitionSystem) (acc: TransitionSystem list) : TransitionSystem list =
    match transitionSystem with
    | TransitionRule(valueExpr, tuples) ->
        // 1. Create the map of named rules
        // 2. traverse valueExpression and replace ;)
        let ruleMap = List.foldBack mapBuilder tuples Map.empty
        let ruleMap' = Map.map (fun _k v -> exprWalker v ruleMap) ruleMap
        TransitionRule(exprWalker valueExpr ruleMap', [])
    | _ -> transitionSystem
    :: acc

let declFolder (decl: Declaration) (acc: Declaration list) : Declaration list =
    match decl with
    | TransitionSystemDeclaration(idPos, transitionSystems) ->
        TransitionSystemDeclaration(idPos, List.foldBack transitionSystemFolder transitionSystems [])
    | _ -> decl
    :: acc

let unfoldNamedTransitionRules1
    (_typeEnv: TypeEnvMap)
    (_valueEnv: ValueEnvMap)
    (cls: Class)
    : TypeEnvMap * ValueEnvMap * Class =
    let cls' = List.foldBack declFolder cls []
    (_typeEnv, _valueEnv, cls')
