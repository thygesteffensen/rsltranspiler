module Transpiler.Transpiler


// let transpiler


let unfold_generic (node: ValueDeclaration) type_env =
    match node with
    | GenericValue(id, typll, typeexpr) -> node
    | _ -> node


let build_symbol_table (_AST: Class) =
    
    let fold acc = function
        | Value _ -> acc
        | TypeDeclaration t -> acc
        
        
    List.fold fold [] _AST