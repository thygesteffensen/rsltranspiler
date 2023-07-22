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

    (List.indexed valueDecl)
    |> List.iter (fun (i, e) ->
        let mapKey x = (i, x)

        match e with
        | ExplicitValue(id, _, _) as ev ->
            match id with
            | ISimple id' -> map <- map.Add(mapKey (fst id'), ev)
            | IGeneric _ -> failwith "todo"
        | ImplicitValue -> failwith "todo"
        | ExplicitFunction -> failwith "todo"
        | ImplicitFunction -> failwith "todo"
        | GenericValue(id, _, _) as gv ->
            match id with
            | ISimple id' -> map <- map.Add(mapKey (fst id'), gv)
            | IGeneric _ -> failwith "todo"
        | Typing(SingleTyping(id, _)) as t ->
            match id with
            | ISimple id' -> map <- map.Add(mapKey (fst id'), t)
            | IGeneric((id', _), _) -> map <- map.Add(mapKey id', t))

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
            | TypeDeclaration td -> { intermediate with Type = Some(td) }
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
        | Some v -> Some(TypeDeclaration v)

    typeDec :: valueDec :: axiomDec :: [ trDec ] |> List.choose id

/// <summary>
/// Build symbol table for given Abstract Syntax Tree and extract type definition type set if Union or sub type
/// </summary>
/// <param name="_AST"></param>
let buildSymbolTable (_AST: Class) : Map<string, TypeDefinition * string list> =

    let unfoldTypeEnvironments acc dec =
        match dec with
        | Value _ -> acc
        | TypeDeclaration ts -> ts @ acc
        | AxiomDeclaration _ -> acc
        | TransitionSystemDeclaration _ -> acc

    let buildType (env: Map<string, TypeDefinition * string list>) =
        function
        | id,
          ((Concrete(Sub([ SingleTyping(ISimple(s0, _), typeExpression) ],
                         Infix(Infix(VName(ASimple(s, _)), GreaterThanOrEqual, ValueLiteral(VInt 0, _)),
                               LogicalAnd,
                               Infix(VName(ASimple(s1, _)), LessThan, ValueLiteral(VInt upperbound, _)))))) as typeDecl) ->
            match typeExpression with
                | TName("Nat", _) -> ()
                | TName("Int", _) -> ()
                | _ -> failwith ""

            if not (s0 = s1 && s1 = s) then
                failwith ""
                
            env.Add((fst id), (typeDecl, (List.map (fun e -> $"_{e}") [ 0..(upperbound-1) ])))
        | id, (Union tuples as typeDecl) ->
            env.Add((fst id), (typeDecl, List.foldBack (fun (e, _pos) a -> $"_{e}" :: a) tuples []))
        | id, typeDecl -> env.Add((fst id), (typeDecl, []))

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

/// <summary>
/// Iterate typings to create unfolded identifier and for each instance the function f is applied yielding 'a 
/// </summary>
/// <param name="typeEnv"></param>
/// <param name="valueEnv"></param>
/// <param name="id"></param>
/// <param name="typings"></param>
/// <param name="f">Function should be called for each final instance of an accessors</param>
/// <param name="acc"></param>
let rec iterateTypings
    (typeEnv: Map<string, TypeDefinition * string list>)
    id
    (typings: Typing list)
    (f: string -> 'a -> 'a)
    (acc: 'a)
    : 'a =
    match typings with
    | [] -> f id acc
        | SingleTyping(ISimple(_, _pos), TName(s, _pos1)) :: ts ->
        match Map.tryFind s typeEnv with
        | None -> failwith "No"
        | Some(_, instances) ->
            List.foldBack (fun e a -> iterateTypings typeEnv $"{id}_{e}" ts f a) instances acc
    | _ -> failwith "Nono"

/// <summary>
/// toString for value literal
/// </summary>
/// <param name="valueLiteral"></param>
let literalToString valueLiteral =
    match valueLiteral with
    | VUnit _ -> "()"
    | VBool b -> string b
    | VInt i -> string i
    | VReal i -> string i
    | VChar c -> string c
    | VNat i -> string i
    | VText s -> s
