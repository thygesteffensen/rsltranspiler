module Transpiler.Helpers

open Transpiler.Ast
open Transpiler.Auxiliary

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
    
let failWithLine (pos: Position) (msg: string) =
    failwith $"{msg} ({pos.pos_lnum}:{pos.pos_cnum} {pos.pos_fname})"

/// <summary>
/// Generic instantiate typings method.
/// Idea:
///     For each quantified expression, the types in the typings must all be instantiated before the first unfolding
///     can happen, i.e.~all types must have a value. Then each combination must be repeated, which is what this
///     function does. It call the baseFunction for each combination. 
/// </summary>
/// <param name="typeEnv">Type Environment</param>
/// <param name="valueTypeEnv">Value Type Environment</param>
/// <param name="valueEnv">Value Environment</param>
/// <param name="typings">Types to be instantiated</param>
/// <param name="accumulator">Accumulator value passed along</param>
/// <param name="element">Element to operate on</param>
/// <param name="baseFunction">The parent function that initiated the instantiation</param>
let rec genericInstantiateTypings
    (typeEnv: TypeEnvMap)
    (valueTypeEnv: Map<Id, TypeExpression>)
    (valueEnv: ValueEnvMap)
    (typings: Typing list)
    (accumulator: 'a)
    (element: 'b)
    (baseFunction: TypeEnvMap -> Map<Id,TypeExpression> -> ValueEnvMap -> 'a -> 'b -> 'a)
    =
    match typings with
    | [] -> baseFunction typeEnv valueTypeEnv valueEnv accumulator element
    | SingleTyping(s, TName typeName) as _ :: ts ->
        match s with
        | ISimple(id, _pos) ->
            match snd (Map.find (fst typeName) typeEnv) with
            | [] -> failwith "Type is infinite and cannot be unfolded."
            | valueLiterals ->
                List.foldBack
                    (fun e a -> genericInstantiateTypings typeEnv valueTypeEnv (Map.add id e valueEnv) ts a element baseFunction)
                    valueLiterals
                    accumulator
        | IGeneric _ -> failwith "todo"
    | _ -> failwith "Only SingleTypings with TypeName type is supported, other types can be added"

let findValue (valueTypeEnv: ValueEnvMap) (valueExpr: ValueExpression) : ValueLiteral =
    match valueExpr with
    | ValueLiteral(valueLiteral, _) -> valueLiteral
    | VName(ASimple(s, _pos)) ->
        match Map.tryFind s valueTypeEnv with
        | None -> failwith $"Cannot compute value of {s}"
        | Some value -> value
    | VName _ -> failwith "todo"
    | VPName _ -> failwith "todo"
    | Rule _ -> failwith "todo"
    | ValueExpression.Quantified _ -> failwith "todo"
    | Infix _ -> failwith "todo"
    | VeList _ -> failwith "todo"
    | VArray _ -> failwith "todo"
    | LogicalNegation _ -> failwith "todo"
    | Prefix(tuple, valueExpression) -> failwith "todo"


let buildValueEnvironment (cls: Class) : ValueEnvMap =
    let getAxioms (decl: Declaration) acc =
        match decl with
        | AxiomDeclaration valueExpressions -> valueExpressions @ acc
        | _ -> acc

    let extractValue1 (valueMap: ValueEnvMap) (valueExpr: ValueExpression) : ValueLiteral =
        match valueExpr with
        | ValueLiteral(valueLiteral, _pos) -> valueLiteral
        | VName _ -> findValue valueMap valueExpr
        | VPName _ -> failwith "todo"
        | Rule _ -> failwith "todo"
        | ValueExpression.Quantified _ -> failwith "todo"
        | Infix _ -> failwith "todo"
        | VeList _ -> failwith "todo"
        | VArray _ -> failwith "todo"
        | LogicalNegation _ -> failwith "todo"
        | Prefix(tuple, valueExpression) -> failwith "todo"

    let extractValue (valueExpr: ValueExpression) (acc: ValueEnvMap) : ValueEnvMap =
        match valueExpr with
        | Infix(VName(ASimple(name, _pos)), Equal, valueExpr) -> Map.add name (extractValue1 acc valueExpr) acc
        | _ -> acc

    let axioms = List.foldBack getAxioms cls []

    List.foldBack extractValue axioms Map.empty


/// <summary>
/// Build symbol table for given Abstract Syntax Tree and extract type definition type set if Union or sub type
/// </summary>
/// <param name="_AST"></param>
/// <param name="valueMap"></param>
let buildSymbolTable (_AST: Class) (valueMap: ValueEnvMap) : TypeEnvMap =

    let unfoldTypeEnvironments acc dec =
        match dec with
        | Value _ -> acc
        | TypeDeclaration ts -> ts @ acc
        | AxiomDeclaration _ -> acc
        | TransitionSystemDeclaration _ -> acc
        | LtlAssertionDeclaration tuples -> failwith "todo"

    let buildType (env: TypeEnvMap) =
        function
        | id,
          ((Concrete(Sub([ SingleTyping(ISimple(s0, _), typeExpression) ],
                         Infix(Infix(VName(ASimple(s, _)), GreaterThanOrEqual, lowerBound),
                               LogicalAnd,
                               Infix(VName(ASimple(s1, _)), LessThan, upperBound))))) as typeDecl) ->
            match typeExpression with
            | TName("Nat", _) -> ()
            | TName("Int", _) -> ()
            | _ -> failwith ""

            if not (s0 = s1 && s1 = s) then
                failwith ""

            let lowerBoundValue =
                match findValue valueMap lowerBound with
                | VInt i
                | VNat i -> i
                | _ -> failwith "Not allowed as bound"

            let upperBoundValue =
                match findValue valueMap upperBound with
                | VInt i
                | VNat i -> i
                | _ -> failwith "Not allowed as bound"

            let ls = [ lowerBoundValue .. (upperBoundValue - 1) ] |> List.map VInt
            env.Add((fst id), (typeDecl, ls))
        | id, (Union tuples as typeDecl) ->
            let ls = List.foldBack (fun (e, _pos) a -> (string e) :: a) tuples [] |> List.map VText
            env.Add((fst id), (typeDecl, ls))
        | id, typeDecl -> env.Add((fst id), (typeDecl, []))

    List.fold unfoldTypeEnvironments [] _AST |> List.fold buildType Map.empty


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

/// <summary>
/// Convert a value expression to a string is possible
/// </summary>
/// <param name="ve"></param>
/// <param name="valueEnv"></param>
let valueExpressionToString (ve: ValueExpression) (valueEnv: ValueEnvMap) =
    match ve with
    | ValueLiteral valueLiteral -> literalToString (fst valueLiteral)
    | VName s ->
        match s with
        | ASimple s -> 
            match Map.tryFind (fst s) valueEnv with
            | None -> fst s // TODO: Should the default just be the string assuming the type checker handles this?
            | Some value -> literalToString value
        | AGeneric _ -> failwith "todo"
    | ValueExpression.Quantified _ -> failwith "todo"
    | VPName _ -> failwith "todo"
    | Rule _ -> failwith "todo"
    | Infix _ -> failwith "todo"
    | VeList _ -> failwith "todo"
    | VArray _ -> failwith "todo"
    | LogicalNegation _ -> failwith "todo"
