module Transpiler.Transpiler


/// <summary>
/// Given a typing list, this will generate a list of all variants of the typing, prefixed with '_' and seperated by '_' .
/// Thus, the typings must be finite, otherwise it will continue forever.
///
/// Given
/// <code>
///     type
///         T1 == t1 | t2,
///         T2 = {| i: Int :- i >=0 /\ i < 3 |}
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
let buildTypePostfixStrings typeEnv valueEnv typingList =
    // Matrix
    let first :: second =
        List.foldBack
            (fun (SingleTyping(_, typeExpr)) acc ->
                match typeExpr with
                | TName n ->
                    match Map.find n typeEnv with
                    | Union l -> (List.foldBack (fun e bb -> e :: bb) l []) :: acc)
            typingList
            []

    let rec buildPostfix (list: string list list) (acc: string list) =
        match list with
        | [] -> List.foldBack (fun e acc -> $"_{e}" :: acc) acc []
        | x :: xs ->
            buildPostfix xs (List.foldBack (fun (p1, p2) acc1 -> $"{p2}_{p1}" :: acc1) (List.allPairs x acc) [])

    buildPostfix second first


/// <summary>
///
/// </summary>
/// <param name="node"></param>
/// <param name="typeEnv"></param>
let unfoldGeneric (node: ValueDeclaration) typeEnv valueEnv =

    match node with
    | GenericValue(id, typingList, typeExpression) ->
        let postfix = buildTypePostfixStrings typeEnv valueEnv typingList

        let names =
            List.foldBack (fun e acc -> SingleTyping($"{id}{e}", typeExpression) :: acc) postfix []

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
        | AxiomDeclaration _ -> acc

    let buildType (env: Map<string, TypeDefinition>) =
        function
        | id, typeDecl -> env.Add(id, typeDecl)

    List.fold unfoldTypeEnvironments [] _AST |> List.fold buildType Map.empty

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

let rec unfoldValueExpr typeEnv valueEnv =
    function
    | ValueLiteral literal as lit -> failwith "todo"
    | VName n as name -> failwith "todo"
    | GenericName(id, valueExpressions) as generic -> failwith "todo"
    | Equivalence(lhs, rhs) -> failwith "todo" //[Equivalence((unfoldValueExpr typeEnv lhs), (unfoldValueExpr typeEnv rhs))]
    | Quantified(_, tl, ve) as qq ->
        // Here we want similar logic when unfolded the generic variables, it's the same accessors we need ;)
        // the quantifier is not useful, only `all` is allowed with is enforced by the type checker
        let prefixes = buildTypePostfixStrings typeEnv valueEnv tl

        match ve with
        | Equivalence(GenericName(name, _), value_expr) ->
            List.foldBack
                (fun e acc -> ExplicitValue($"{name}{e}", (Map.find name valueEnv), value_expr) :: acc)
                prefixes
                []
        | _ -> failwith "not supported"

let unfoldValue typeEnv valueEnv valueDeclaration =
    match valueDeclaration with
    | ExplicitValue(id, typeExpr, valueExpr) -> [ ExplicitValue(id, typeExpr, valueExpr) ]
    | ImplicitValue -> failwith "todo"
    | ExplicitFunction -> failwith "todo"
    | ImplicitFunction -> failwith "todo"
    | GenericValue(id, typingList, typeExpr) as gv -> unfoldGeneric gv typeEnv valueEnv
    | Typing typing -> failwith "todo"


let unfoldType typeEnv valueEnv (id, typeDefinition as t) =
    match typeDefinition with
    | Abstract -> failwith "todo"
    | Concrete typeExpr -> failwith "todo"
    | Union idList -> [ (id, Union idList) ]

let unfoldDeclaration typeEnv valueEnv decl =
    match decl with
    | Value valueDeclarations ->
        Value(List.foldBack (fun valueDecl acc -> (unfoldValue typeEnv valueEnv valueDecl) @ acc) valueDeclarations [])
    | TypeDeclaration typeDeclarations ->
        TypeDeclaration(
            List.foldBack (fun typeDecl acc -> (unfoldType typeEnv valueEnv typeDecl) @ acc) typeDeclarations []
        )
    // We need to remove typings if present?
    | AxiomDeclaration axioms ->
        Value(List.foldBack (fun e acc -> (unfoldValueExpr typeEnv valueEnv e) @ acc) axioms [])

let unfoldClass typeEnv valueEnv cls =
    List.foldBack (fun declaration acc -> (unfoldDeclaration typeEnv valueEnv declaration) :: acc) cls []

[<Struct>]
type Intermediate =
    { Type: Declaration
      Value: Map<string, ValueDeclaration>
      Axiom: Declaration }

let rec convertToIntermediate (cls: Class) (intermediate: Intermediate) =
    match cls with
    | [] -> intermediate
    | decl :: decls ->
        let intermediate' =
            match decl with
            | Value valueDeclarations ->
                let mutable map = intermediate.Value

                valueDeclarations
                |> List.iter (fun e ->
                    match e with
                    | ExplicitValue(id, _, _) as ev -> map <- map.Add(id, ev)
                    | ImplicitValue -> failwith "todo"
                    | ExplicitFunction -> failwith "todo"
                    | ImplicitFunction -> failwith "todo"
                    | GenericValue(id, _, _) as gv -> map <- map.Add(id, gv)
                    | Typing typing -> failwith "todo")

                { intermediate with Value = map }
            | TypeDeclaration _ as td -> { intermediate with Type = td }
            | AxiomDeclaration _ as ad -> { intermediate with Axiom = ad }

        convertToIntermediate decls intermediate'

let unfoldAxioms typeEnv valueEnv (intermediate: Intermediate) =
    let mutable map = intermediate.Value

    let unfoldValueExpression =
        function
        | Quantified(_, tl, ve) ->
            let prefixes = buildTypePostfixStrings typeEnv valueEnv tl

            match ve with
            | Equivalence(GenericName(name, _), value_expr) ->
                let valueType = Map.find name valueEnv

                prefixes
                |> List.iter (fun e -> map <- map.Add($"{name}{e}", ExplicitValue($"{name}{e}", valueType, value_expr)))
            | _ -> failwith "Not supported in axiom unfolding."
        | _ -> failwith "Not supported in axiom unfolding."

    match intermediate with
    | { Type = _
        Value = _
        Axiom = axiomDecl } ->
        match axiomDecl with
        | Value _ -> failwith "todo"
        | TypeDeclaration _ -> failwith "todo"
        | AxiomDeclaration axioms ->
            axioms
            |> List.iter unfoldValueExpression

let transpile ((specification, cls): Scheme) =
    let typeEnvironment = buildSymbolTable cls
    let valueEnvironment = buildValueTable cls

    let intermediate =
        convertToIntermediate
            cls
            { Type = TypeDeclaration []
              Value = Map.empty
              Axiom = AxiomDeclaration [] }

    let unfolded = unfoldClass typeEnvironment valueEnvironment cls

    Scheme($"{specification}_unfolded", unfolded)
