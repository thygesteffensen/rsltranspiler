module Transpiler.RuleCollection.Helpers

open Transpiler.Ast
open Transpiler.Helpers.Helpers
open Transpiler.Intermediate

/// <summary>
/// We allow unfolding value expression until the
/// </summary>
/// <param name="typeEnv"></param>
/// <param name="valueTypeEnv"></param>
/// <param name="map"></param>
/// <param name="valueEnv"></param>
/// <param name="axiom"></param>
let rec axiomFolder
    typeEnv
    valueTypeEnv
    (valueEnv: ValueEnvMap)
    (map: ValueDecMap)
    (axiom: IrAxiomDeclaration)
    : ValueDecMap =
    match axiom with
    | IrQuantified(typings, valueExpr) ->
        genericInstantiateTypings typeEnv valueTypeEnv valueEnv typings map valueExpr axiomFolder
    | IrInfix(identifier, rhs) ->
        let findIndex (key: string) : int * string =
            let i, _ = Map.findKey (fun (_i, k) _v -> k = key) map
            (i, key)

        let rhs' = unfoldValueExpression typeEnv valueTypeEnv valueEnv rhs

        match identifier with
        | ASimple(s, pos) ->
            let valueType = Map.find s valueTypeEnv
            map.Add(findIndex s, ExplicitValue(Identifier.ISimple(s, pos), valueType, rhs'))
        | AGeneric((s, pos), valueExpressions) ->
            let valueType = Map.find s valueTypeEnv

            let postfix =
                List.foldBack (fun e a -> $"_{valueExpressionToString e valueEnv}{a}") valueExpressions ""

            let identifier = $"{s}{postfix}"
            let i, _k = findIndex s

            map.Add((i, identifier), ExplicitValue(ISimple(identifier, pos), valueType, rhs'))
