module Transpiler.Helpers.Helpers

open Transpiler.Helpers.IrTransformer
open Transpiler.Ast
open Transpiler.Intermediate

open FSharp.Text.Lexing

let dp: Position =
    { pos_bol = 1
      pos_cnum = 1
      pos_orig_lnum = 1
      pos_fname = ""
      pos_lnum = 1 }
    
let error_handler (tok: FSharp.Text.Parsing.ParseErrorContext<_>) : unit =
    printfn $"Current token: {tok.CurrentToken} and {tok.ShiftTokens}"
    ()

let convertAxiomDeclToIr (valueExprList: ValueExpression list) =

    let rec valueExpressionToIr (valueExpr: ValueExpression) =
        match valueExpr with
        | ValueExpression.Quantified((All, _), typings, valueExpression) ->
            IrQuantified(typings, valueExpressionToIr valueExpression)
        | Infix(VName accessor, Equal, expression) -> IrInfix(accessor, expression)
        // TODO: Expression should be further walked to ensure it does not violate rules.
        // E.g., it cannot contain quantified generic expression. But then again, that should just evaluate to a
        // boolean

        | ValueExpression.Quantified _ -> failwith "Quantified expression must use the all quantifier"
        | Infix _ -> failwith "Infix expression in axioms must be on the form: <Accessor> = <ValueExpr>"

        | _ -> failwith "Axioms can only be a Quantified or Infix expression"


    List.foldBack (fun e a -> valueExpressionToIr e :: a) valueExprList []

let convertValueDeclToIr value valueDecl =
    let mutable map =
        match value with
        | None -> Map.empty
        | Some m -> m

    valueDecl
    |> List.iter (fun e ->
        match e with
        | ExplicitValue(id, _, _) as ev ->
            match id with
            | ISimple id' -> map <- map.Add((fst id'), ev)
            | IGeneric _ -> failwith "todo"
        | ImplicitValue -> failwith "todo"
        | ExplicitFunction -> failwith "todo"
        | ImplicitFunction -> failwith "todo"
        | GenericValue(id, _, _) as gv ->
            match id with
            | ISimple id' -> map <- map.Add((fst id'), gv)
            | IGeneric _ -> failwith "todo"
        | Typing(SingleTyping(id, _)) as t ->
            match id with
            | ISimple id' -> map <- map.Add((fst id'), t)
            | IGeneric((id', _), _) -> map <- map.Add(id', t))

    map

/// <summary>
/// Convert AST to intermediate representation.
///
/// This is used before unfolding the specification.
/// </summary>
/// <param name="cls"></param>
/// <param name="intermediate"></param>
let rec convertToIntermediate (cls: Class) (intermediate: Intermediate) =
    match cls with
    | [] -> intermediate
    | decl :: decls ->
        let intermediate' =
            match decl with
            | Value valueDeclarations ->
                let value = convertValueDeclToIr intermediate.Value valueDeclarations

                { intermediate with
                    Value = Some(value) }
            | TypeDeclaration _ as td -> { intermediate with Type = Some(td) }
            | AxiomDeclaration ad ->
                { intermediate with
                    Axiom = Some(convertAxiomDeclToIr ad) }
            | TransitionSystemDeclaration(idPos, transitionSystems) ->
                { intermediate with
                    TransitionSystem = Some(convertAstTransitionSystemToIr idPos transitionSystems) }

        convertToIntermediate decls intermediate'

let rec axiomIrToAst (a: IrAxiomDeclaration) =
    match a with
    | IrQuantified(typings, irAxiomDeclaration) ->
        ValueExpression.Quantified((All, dummyPos), typings, axiomIrToAst irAxiomDeclaration)
    | IrInfix(accessor, valueExpression) -> Infix(VName accessor, Equal, valueExpression)


/// <summary>
/// Convert Intermediate representation back to AST.
///
/// This is used when writing the specification to any source language
/// </summary>
/// <param name="intermediate"></param>
let rec convertToAst (intermediate: Intermediate) =
    let trDec =
        match intermediate.TransitionSystem with
        | None -> None
        | Some v -> Some(convertIrTransitionSystemToAst v)

    let axiomDec =
        match intermediate.Axiom with
        | None -> None
        | Some v -> Some(AxiomDeclaration(List.foldBack (fun e a -> (axiomIrToAst e) :: a) v []))

    let valueDec =
        match intermediate.Value with
        | Some v when Map.empty <> v ->
            let value = Map.foldBack (fun _ v a -> v :: a) v []
            Some(Value value)
        | _ -> None

    let typeDec =
        match intermediate.Type with
        | None -> None
        | Some v -> Some(v)

    typeDec :: valueDec :: axiomDec :: [ trDec ] |> List.choose id

/// <summary>
/// Build symbol table for given Abstract Syntax Tree
/// </summary>
/// <param name="_AST"></param>
let buildSymbolTable (_AST: Class) =

    let unfoldTypeEnvironments acc dec =
        match dec with
        | Value _ -> acc
        | TypeDeclaration ts -> ts @ acc
        | AxiomDeclaration _ -> acc
        | TransitionSystemDeclaration _ -> acc

    let buildType (env: Map<string, TypeDefinition>) =
        function
        | id, typeDecl -> env.Add((fst id), typeDecl)

    List.fold unfoldTypeEnvironments [] _AST |> List.fold buildType Map.empty

/// <summary>
/// Build a lookup table for looking up the value type.
/// </summary>
/// <param name="_AST"></param>
let buildValueTable (_AST: Class) =

    let unfoldValueEnvironments acc =
        function
        | Value v -> v @ acc
        | _ -> acc

    let unfoldValueValues (map: Map<string, TypeExpression>) valueExpr =
        match valueExpr with
        | ExplicitValue(s, typeExpression, _) ->
            match s with
            | ISimple s' -> map.Add((fst s'), typeExpression)
            | IGeneric _ -> failwith "todo"

        | GenericValue(s, _, typeExpression) ->
            match s with
            | ISimple s' -> map.Add((fst s'), typeExpression)
            | IGeneric _ -> failwith "todo"
        | Typing(SingleTyping(id, typeExpr)) ->
            match id with
            | ISimple(id', _)
            | IGeneric((id', _), _) -> map.Add(id', typeExpr)
        | _ -> map

    List.fold unfoldValueEnvironments [] _AST
    |> List.fold unfoldValueValues Map.empty
