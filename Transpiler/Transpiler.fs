module Transpiler.Transpiler


// let transpiler


let unfold_generic (node: ValueDeclaration) type_env =
    match node with
    | GenericValue(id, typll, typeexpr) -> node
    | _ -> node


let buildSymbolTable (_AST: Class) =
    
    let unfoldTypeEnvironments acc = function
        | Value _ -> acc
        | TypeDeclaration ts -> ts @ acc
    
    let buildType (env: Map<string, TypeDefinition>) = function
        | id, typeDecl -> env.Add(id, typeDecl)
    
    List.fold unfoldTypeEnvironments [] _AST |> List.fold buildType Map.empty