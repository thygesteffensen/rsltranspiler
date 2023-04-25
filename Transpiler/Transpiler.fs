module Transpiler.Transpiler



/// <summary>
///
/// </summary>
/// <param name="node"></param>
/// <param name="type_env"></param>
let unfoldGeneric (node: ValueDeclaration) type_env =

    /// <summary>
    /// This should construct a list of postfix for the typing array
    /// </summary>
    /// <param name="id"></param>
    /// <param name="typeExpr"></param>
    /// <param name="acc"></param>
    let folder typingList =
        // Matrix
        let first :: second =
            List.foldBack
                (fun (SingleTyping(_, typeExpr)) acc ->
                    match typeExpr with
                    | Name n ->
                        match Map.find n type_env with
                        | Union l -> (List.foldBack (fun e bb -> e :: bb) l []) :: acc)
                typingList
                []


        let rec buildPostfix (list: string list list) (acc: string list) =
            match list with
            | [] -> acc
            | x :: xs ->
                buildPostfix xs (List.foldBack (fun (p1, p2) acc1 -> $"{p1}_{p2}" :: acc1) (List.allPairs x acc) [])

        buildPostfix second first

    match node with
    | GenericValue(id, typingList, typeExpression) ->
        let postfix = folder typingList

        let names =
            List.foldBack (fun e acc -> SingleTyping($"{id}_{e}", typeExpression) :: acc) postfix []

        List.foldBack (fun e acc -> (Typing e) :: acc) names []

    | _ -> [ node ]


/// <summary>
/// Build symbol table for given Abstract Syntax Tree
/// </summary>
/// <param name="_AST"></param>
let buildSymbolTable (_AST: Class) =

    let unfoldTypeEnvironments acc =
        function
        | Value _ -> acc
        | TypeDeclaration ts -> ts @ acc

    let buildType (env: Map<string, TypeDefinition>) =
        function
        | id, typeDecl -> env.Add(id, typeDecl)

    List.fold unfoldTypeEnvironments [] _AST |> List.fold buildType Map.empty

let unfoldValue typeEnv valueDeclaration =
    match valueDeclaration with
    | ExplicitValue(id, typeExpr, valueExpr) -> [ExplicitValue(id, typeExpr, valueExpr)]
    | ImplicitValue -> failwith "todo"
    | ExplicitFunction -> failwith "todo"
    | ImplicitFunction -> failwith "todo"
    | GenericValue(id, typingList, typeExpr) as gv -> unfoldGeneric gv typeEnv
    | Typing typing -> failwith "todo"


let unfoldType typeEnv (id, typeDefinition as t) =
    match typeDefinition with
    | Abstract -> failwith "todo"
    | Concrete typeExpr -> failwith "todo"
    | Union idList  -> [(id, Union idList)]

let unfoldDeclaration typeEnv decl =
    match decl with
    | Value valueDeclarations ->
        Value(List.foldBack (fun valueDecl acc -> (unfoldValue typeEnv valueDecl) @ acc) valueDeclarations [])
    | TypeDeclaration typeDeclarations ->
        TypeDeclaration(List.foldBack (fun typeDecl acc -> (unfoldType typeEnv typeDecl) @ acc) typeDeclarations [])

let unfoldClass typeEnv cls =
    List.foldBack (fun declaration acc -> (unfoldDeclaration typeEnv declaration) :: acc) cls []



let transpile ((specification, cls): Scheme) =
    let typeEnvironment = buildSymbolTable cls
    let unfolded = unfoldClass typeEnvironment cls

    Scheme($"{specification}_unfolded", unfolded)
