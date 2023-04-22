module Transpiler.Transpiler


// let transpiler


let unfold_generic (node: ValueDeclaration) type_env =
    match node with
    | GenericValue(id, typll, typeexpr) -> node
    | _ -> node


