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
    
    
/// <summary>
/// Given a typing list, this will generate a list of all variants of the typing, prefixed with '_' and seperated by '_' .
/// Thus, the typings must be finite, otherwise it will continue forever.
///
/// Given
/// <code>
///     type
///         T1 == t1 | t2,
///         T2 = {| i: Int :- i >=0 /\ i $gt 3 |}
/// </code>
/// this would generate:
/// <code>
///     [ _t1_0
///       _t1_1
///       _t1_2
///       _t2_0
///       _t2_1
///       _t2_3 ]
/// </code>
///
/// </summary>
/// <param name="typingList">The typing list for which the post fixes are generated</param>
/// <param name="typeEnv">Type environment</param>
/// <param name="valueEnv">Value environment</param>
let buildTypePostfixStrings typeEnv valueEnv typingList =

    // Matrix
    let first :: second =
        List.foldBack
            (fun (SingleTyping(_, typeExpr)) acc ->
                match typeExpr with
                | TName n ->
                    match Map.find (fst n) typeEnv with
                    | Union l -> (List.foldBack (fun (e, _) bb -> e :: bb) l []) :: acc)
            typingList
            []

    let rec buildPostfix (list: string list list) (acc: string list) =
        match list with
        | [] -> List.foldBack (fun e acc -> $"_{e}" :: acc) acc []
        | x :: xs ->
            buildPostfix xs (List.foldBack (fun (p1, p2) acc1 -> $"{p2}_{p1}" :: acc1) (List.allPairs x acc) [])

    buildPostfix second first
