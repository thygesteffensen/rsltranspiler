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

let failWithLine (pos: Position) (msg: string) =
    failwith $"{msg} ({pos.pos_lnum}:{pos.pos_cnum} {pos.pos_fname})"

let convertAxiomDeclToIr (valueExprList: ValueExpression list) =

    let rec valueExpressionToIr (valueExpr: ValueExpression) =
        match valueExpr with
        | ValueExpression.Quantified((All, _), typings, valueExpression) ->
            IrQuantified(typings, valueExpressionToIr valueExpression)
        | Infix(VName accessor, Equal, expression) -> IrInfix(accessor, expression)
        | ValueExpression.Quantified _ -> failwith "Quantified expression must use the all quantifier"
        | Infix _ -> failwith "Infix expression in axioms must be on the form: <Accessor> = <ValueExpr>"

        | _ -> failwith "Axioms can only be a Quantified or Infix expression"


    List.foldBack (fun e a -> valueExpressionToIr e :: a) valueExprList []

/// <summary>
/// Convert Value declarations to Intermediate representation
/// </summary>
/// <param name="valueDeclarationsOption"></param>
/// <param name="valueDecl"></param>
let convertValueDeclToIr (valueDeclarationsOption: Option<ValueDecMap>) (valueDecl: ValueDeclaration list) =
    let mutable map =
        match valueDeclarationsOption with
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
        | Typing(SingleTyping(id, _)) as t ->
            match id with
            | ISimple id' -> map <- map.Add(mapKey (fst id'), t)
            | IGeneric((id', _), _) -> map <- map.Add(mapKey id', t))

    map

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
    (baseFunction: TypeEnvMap -> Map<Id, TypeExpression> -> ValueEnvMap -> 'a -> 'b -> 'a)
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
                    (fun e a ->
                        genericInstantiateTypings
                            typeEnv
                            valueTypeEnv
                            (Map.add id e valueEnv)
                            ts
                            a
                            element
                            baseFunction)
                    valueLiterals
                    accumulator
        | IGeneric _ -> failwith "todo"
    | _ -> failwith "Only SingleTypings with TypeName type is supported, other types can be added"

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
            | LtlAssertionDeclaration tuples ->
                { intermediate with
                    LtlAssertion = Some(tuples) }

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
    let ltlDec =
        match intermediate.LtlAssertion with
        | None -> None
        | Some v -> Some(LtlAssertionDeclaration v)

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

    typeDec :: valueDec :: axiomDec :: trDec :: [ ltlDec ] |> List.choose id

let findValue (valueEnv: ValueEnvMap) (valueExpr: ValueExpression) : ValueLiteral =
    match valueExpr with
    | ValueLiteral(valueLiteral, _) -> valueLiteral
    | VName(ASimple(s, _pos))
    | VName(AGeneric((s, _pos), _)) -> // Should the default just be the string assuming the type checker handles this?
        match Map.tryFind s valueEnv with
        | None -> VText s
        | Some value -> value
    | VPName(ASimple(_, pos))
    | VPName(AGeneric((_, pos), _)) -> failWithLine pos "Primed names cannot have a value."
    | Rule(_, pos) -> failWithLine pos "Rule cannot have a value."
    | ValueExpression.Quantified((_, pos), _, _) -> failWithLine pos "Quantified expression cannot have a value."
    | Infix _ -> failwith "Rule cannot have a value."
    | VeList _ -> failwith "VeList cannot have a value."
    | VArray _ -> failwith "VArray cannot have a value."
    | LogicalNegation(_, pos) -> failWithLine pos "Rule cannot have a value."
    | Prefix((_, pos), _) -> failWithLine pos "Rule cannot have a value."

let buildValueEnvironment (cls: Class) : ValueEnvMap =
    let valueEnvClassFolder (decl: Declaration) (env: ValueEnvMap) : ValueEnvMap =
        match decl with
        | Value valueDeclarations ->
            let valueDeclFolder (decl: ValueDeclaration) (env: ValueEnvMap) : ValueEnvMap =
                match decl with
                | ExplicitValue(ISimple(mapKey, _pos), _, valueExpr) ->
                    match valueExpr with
                    | ValueLiteral(valueLiteral, _pos) -> Map.add mapKey valueLiteral env
                    | VName(ASimple(identifier, _pos)) ->
                        match Map.tryFind identifier env with
                        | None -> env
                        | Some value -> Map.add mapKey value env
                    | _ -> env // Non other type can have value, but it is okay
                // TODO: Determine when infix 1 + 1 should be computed and added
                | _ -> env

            List.foldBack valueDeclFolder valueDeclarations env
        | AxiomDeclaration axioms ->
            List.foldBack
                (fun e acc ->
                    match e with
                    | Infix(VName(ASimple(name, _pos)), Equal, valueExpr) -> Map.add name (findValue acc valueExpr) acc
                    | _ -> acc)
                axioms
                env
        | _ -> env

    List.foldBack valueEnvClassFolder cls Map.empty

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
        | LtlAssertionDeclaration _ -> acc

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
            let ls =
                List.foldBack (fun (e, _pos) a -> (string e) :: a) tuples [] |> List.map VText

            env.Add((fst id), (typeDecl, ls))
        | id, typeDecl -> env.Add((fst id), (typeDecl, []))

    List.fold unfoldTypeEnvironments [] _AST |> List.fold buildType Map.empty

/// <summary>
/// Build a lookup table for looking up the value type.
/// </summary>
/// <param name="_AST"></param>
let buildValueTypeTable (_AST: Class) =

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
        | Typing(SingleTyping(id, typeExpr)) ->
            match id with
            | ISimple(id', _)
            | IGeneric((id', _), _) -> map.Add(id', typeExpr)

    List.fold unfoldValueEnvironments [] _AST
    |> List.fold unfoldValueValues Map.empty

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
/// Iterate typings to create unfolded identifier and for each instance the function f is applied yielding 'a
/// </summary>
/// <param name="typeEnv"></param>
/// <param name="id"></param>
/// <param name="typings"></param>
/// <param name="f">Function should be called for each final instance of an accessors</param>
/// <param name="acc"></param>
let rec iterateTypings (typeEnv: TypeEnvMap) id (typings: Typing list) (f: string -> 'a -> 'a) (acc: 'a) : 'a =
    match typings with
    | [] -> f id acc
    | SingleTyping(ISimple(_, _pos), TName(s, _pos1)) :: ts ->
        match Map.tryFind s typeEnv with
        | None -> failwith "No"
        | Some(_, instances) ->
            let l = List.map literalToString instances
            List.foldBack (fun e a -> iterateTypings typeEnv $"{id}_{e}" ts f a) l acc
    | _ -> failwith "No no"

/// <summary>
/// Convert a value expression to a string is possible
/// </summary>
/// <param name="ve"></param>
/// <param name="valueEnv"></param>
let valueExpressionToString (ve: ValueExpression) (valueEnv: ValueEnvMap) = literalToString (findValue valueEnv ve)

let unfoldAccessor
    _typeEnv
    _valueTypeEnv
    (valueEnv: ValueEnvMap)
    (accessor: Accessor)
    (f: Accessor -> 'a)
    (f1: ValueExpression -> 'a)
    : 'a =
    match accessor with
    | ASimple(id, position) ->
        match Map.tryFind id valueEnv with
        | None -> f accessor
        | Some value -> f1 (ValueLiteral(value, position))
    | AGeneric((id, pos), valueExprs) ->
        let postfix =
            List.foldBack (fun e a -> (valueExpressionToString e valueEnv) + a) valueExprs ""

        f (ASimple(id + "_" + postfix, pos))

let rec unfoldValueExpression
    (typeEnv: TypeEnvMap)
    valueTypeEnv
    (valueEnv: ValueEnvMap)
    (v: ValueExpression)
    : ValueExpression =

    match v with
    | ValueExpression.Quantified((quantifier, _pos), typings, valueExpression) ->
        let delimiter =
            match quantifier with
            | All -> LogicalAnd
            | Exists -> LogicalAnd
            | ExactlyOne -> failwith "ExactlyOne is not supported"
            | Quantifier.Deterministic -> InfixOp.Deterministic
            | Quantifier.NonDeterministic -> InfixOp.NonDeterministic


        let rec typingFolder
            (typings: Typing list)
            (valueEnv': ValueEnvMap)
            (acc: ValueExpression list)
            : ValueExpression list =
            match typings with
            | SingleTyping(ISimple(id, _pos), TName(tName, _pos1)) :: ts ->
                match Map.tryFind tName typeEnv with
                | None -> failwith $"Could not find {tName} in type environment"
                | Some(_typeDef, instances) ->
                    List.foldBack (fun instance a -> typingFolder ts (Map.add id instance valueEnv') a) instances acc
            | [] -> unfoldValueExpression typeEnv valueTypeEnv valueEnv' valueExpression :: acc
            | _ -> failwith "Given typing not supported"

        let l = typingFolder typings valueEnv [] // Yields a list of the value expressions
        let ll = List.reduce (fun e a -> Infix(e, delimiter, a)) l // List reduced to a single infix expression

        ll
    | Infix(lhs, infixOp, rhs) ->
        let lhs' = unfoldValueExpression typeEnv valueTypeEnv valueEnv lhs
        let rhs' = unfoldValueExpression typeEnv valueTypeEnv valueEnv rhs
        Infix(lhs', infixOp, rhs')
    | VeList l ->
        List.foldBack (fun e a -> (unfoldValueExpression typeEnv valueTypeEnv valueEnv e) :: a) l []
        |> VeList
    | VName accessor -> unfoldAccessor typeEnv valueTypeEnv valueEnv accessor VName id
    | VPName accessor -> unfoldAccessor typeEnv valueTypeEnv valueEnv accessor VPName id
    | ValueLiteral _ -> v
    | Rule _ -> v
    | VArray valueExpressions ->
        List.foldBack (fun e a -> unfoldValueExpression typeEnv valueTypeEnv valueEnv e :: a) valueExpressions []
        |> VArray
    | LogicalNegation(valueExpression, position) ->
        LogicalNegation(unfoldValueExpression typeEnv valueTypeEnv valueEnv valueExpression, position)
    | Prefix(tuple, valueExpression) ->
        Prefix(tuple, unfoldValueExpression typeEnv valueTypeEnv valueEnv valueExpression)
