module Transpiler.Auxiliary

open FSharp.Text.Lexing
open Transpiler.Ast

let rec getPosFromValueExpression (valueExpr: ValueExpression) : Position =
    let getPosFromAccessor (accessor: Accessor) : Position =
        match accessor with
        | ASimple(_, position)
        | AGeneric((_, position), _) -> position

    match valueExpr with
    | ValueExpression.Quantified((_, position), _, _) -> position
    | ValueLiteral(_, position) -> position
    | Rule(_, position) -> position
    | VName accessor -> getPosFromAccessor accessor
    | VPName accessor -> getPosFromAccessor accessor
    | Infix(ve, _, _) -> getPosFromValueExpression ve
    | VeList(ve :: _) -> getPosFromValueExpression ve
    | VeList([]) -> failwith "Value Expression cannot be empty - location unknown..."
    | VArray(ve :: _) -> getPosFromValueExpression ve
    | VArray [] -> failwith "Value Expression cannot be empty - location unknown..."
    | LogicalNegation(_, position) -> position
    | Prefix((_, position), _) -> position
