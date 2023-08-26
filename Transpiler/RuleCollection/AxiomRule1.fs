module Transpiler.RuleCollection.AxiomRule1

open Transpiler.Ast
open Transpiler.Intermediate
open Transpiler.Helpers.Helpers

type AxiomMap = Map<Id, ValueExpression>

let rec axiomMapValueExprHandler (valueExpr: ValueExpression) (acc: AxiomMap) : AxiomMap =
    match valueExpr with
    | Infix(VName(ASimple(name, _pos)), Equal, valueExpr) -> acc.Add(name, valueExpr)
    | Infix(_, Equal, _) -> failwith "Right hand side of equal infix must be Simple name"
    | Infix(valueExpr1, LogicalAnd, valueExpr2) ->
        axiomMapValueExprHandler valueExpr1 acc |> axiomMapValueExprHandler valueExpr2
    | Infix _ -> failwith "Only infix expression with the Equal and LogicalAnd operator allowed"
    | _ -> failwith "Only infix expressions allowed in Axiom Declaration"

let buildAxiomMap (decl: Declaration) (acc: AxiomMap) : AxiomMap =
    match decl with
    | AxiomDeclaration valueExpressions -> List.foldBack axiomMapValueExprHandler valueExpressions acc
    | _ -> acc


let valueDeclFolder
    (axiomMap: AxiomMap)
    (valueDecl: ValueDeclaration)
    (acc: ValueDeclaration list)
    : ValueDeclaration list =
    match valueDecl with
    | ExplicitValue _ -> valueDecl // Already explicit ;)
    | Typing(SingleTyping(IGeneric((_, pos), _), _)) ->
        failWithLine pos "Generic identifiers are not allowed at this stage"
    | Typing(SingleTyping(ISimple(name, pos), typeExpr)) ->
        match Map.tryFind name axiomMap with
        | None -> failWithLine pos $"{name} is expected to have a accompanying axiom"
        | Some value -> ExplicitValue(ISimple(name, pos), typeExpr, value)
    :: acc

let makeEmImplicit (axiomMap: AxiomMap) (decl: Declaration) (acc: Declaration list) : Declaration list =
    match decl with
    | Value valueDeclarations -> (List.foldBack (valueDeclFolder axiomMap) valueDeclarations [] |> Value) :: acc
    | AxiomDeclaration _ -> acc // Axioms are discarded in the process
    | _ -> decl :: acc

let makeValueDeclarationExplicit
    (_typeEnv: TypeEnvMap)
    (_valueEnv: ValueEnvMap)
    (cls: Class)
    : TypeEnvMap * ValueEnvMap * Class =
    // 1. Gather axioms?
    // 2. Make implicit?
    let axiomMap = List.foldBack buildAxiomMap cls Map.empty
    let cls' = List.foldBack (makeEmImplicit axiomMap) cls []

    (_typeEnv, _valueEnv, cls')
