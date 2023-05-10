
module Transpiler.Helpers

let error_handler (tok: FSharp.Text.Parsing.ParseErrorContext<_>): unit =
    printfn $"Current token: {tok.CurrentToken} and {tok.ShiftTokens}"
    ()

/// <summary>
/// Convert AST to intermediate representation.
///
/// This is used before unfolding the speciciation.
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
                let mutable map =
                    match intermediate.Value with
                    | None -> Map.empty
                    | Some m -> m

                valueDeclarations
                |> List.iter (fun e ->
                    match e with
                    | ExplicitValue(id, _, _) as ev -> map <- map.Add(id, ev)
                    | ImplicitValue -> failwith "todo"
                    | ExplicitFunction -> failwith "todo"
                    | ImplicitFunction -> failwith "todo"
                    | GenericValue(id, _, _) as gv -> map <- map.Add(id, gv)
                    | Typing _ -> failwith "todo")

                { intermediate with Value = Some(map) }
            | TypeDeclaration _ as td -> { intermediate with Type = Some(td) }
            | AxiomDeclaration _ as ad -> { intermediate with Axiom = Some(ad) }

        convertToIntermediate decls intermediate'

/// <summary>
/// Convert Intermediate representation back to AST.
///
/// This is used when writing the specification to any source language
/// </summary>
/// <param name="intermediate"></param>
/// <param name="acc"></param>
let rec convertToAst (intermediate: Intermediate) (acc: Class) =
    let acc1 =
        match intermediate.Axiom with
        | None -> acc
        | Some v -> v :: acc

    let acc2 =
        match intermediate.Value with
        | None -> acc
        | Some v ->
            let value = Map.foldBack (fun _ v a -> v :: a) v []
            (Value value) :: acc1

    match intermediate.Type with
    | None -> acc2
    | Some v -> v :: acc2
    
/// <summary>
/// Build symbol table for given Abstract Syntax Tree
/// </summary>
/// <param name="_AST"></param>
let buildSymbolTable (_AST: Class) =

    let unfoldTypeEnvironments acc =
        function
        | Value _ -> acc
        | TypeDeclaration ts -> ts @ acc
        | AxiomDeclaration _ -> acc

    let buildType (env: Map<string, TypeDefinition>) =
        function
        | id, typeDecl -> env.Add(id, typeDecl)

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

    let unfoldValueValues (map: Map<string, TypeExpression>) =
        function
        | ExplicitValue(s, typeExpression, _) -> map.Add(s, typeExpression)
        | GenericValue(s, _, typeExpression) -> map.Add(s, typeExpression)
        | _ -> map

    List.fold unfoldValueEnvironments [] _AST
    |> List.fold unfoldValueValues Map.empty