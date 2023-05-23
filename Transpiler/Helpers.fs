module Transpiler.Helpers

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

let convertAxiomDeclToIr (valueExprList: ValueExpression list) =

    let rec valueExpressionToIr (valueExpr: ValueExpression) =
        match valueExpr with
        | Quantified(All, typings, valueExpression) -> IrQuantified(typings, valueExpressionToIr valueExpression)
        | Infix(VName accessor, Equal, expression) ->
            IrInfix(accessor, expression)
            // TODO: Expression should be further walked to ensure it does not violate rules.
            // E.g., it cannot contain quantified generic expression. But then again, that should just evaluate to a
            // boolean

        | Quantified _ -> failwith "Quantified expression must use the all quantifier"
        | Infix _ -> failwith "Infix expression in axioms must be on the form: <Accessor> = <ValueExpr>"

        | _ -> failwith "Axioms can only be a Quantified or Infix expression"


    List.foldBack (fun e a -> valueExpressionToIr e :: a) valueExprList []

let convertValueDeclToIr value valueDecl=
    let mutable map =
        match value with
        | None -> Map.empty
        | Some m -> m

    valueDecl
    |> List.iter (fun e ->
        match e with
        | ExplicitValue(id, _, _) as ev ->
            match id with
            | ISimple id' -> map <- map.Add((fst id'), ev)
            | IGeneric _ -> failwith "todo"
        | ImplicitValue -> failwith "todo"
        | ExplicitFunction -> failwith "todo"
        | ImplicitFunction -> failwith "todo"
        | GenericValue(id, _, _) as gv ->
            match id with
            | ISimple id' -> map <- map.Add((fst id'), gv)
            | IGeneric _ -> failwith "todo"
        | Typing(SingleTyping(id, _)) as t -> 
            match id with
            | ISimple id' -> map <- map.Add((fst id'), t)
            | IGeneric((id', _), _) -> map <- map.Add(id', t))
    
    map

/// <summary>
/// Convert value expression to IrTr
/// </summary>
/// <param name="valueExpr"></param>
let rec convertValueExpressionToIrRule (valueExpr: ValueExpression) =
    // TODO: Clean up
    match valueExpr with
    | Quantified(q, typings, valueExpression) ->
        match q with
        | Quantifier.NonDeterministic -> Quan(NonDeterministic, typings, convertValueExpressionToIrRule valueExpression)
        | Quantifier.Deterministic -> Quan(Deterministic, typings, convertValueExpressionToIrRule valueExpression)
        | _ ->
            failwith
                "Quantified expression are only allowed to be quantified over the deterministic or non deterministic choice, [=] and [>] respectively"
    | Infix(guard, infixOp, effect) ->
        match infixOp with
        | Guard ->
            match effect with
            | Infix(VPName accessor, Eq, rhs) ->
                Guarded
                    { Guard = guard
                      Effect = [ (accessor, rhs) ] }
            | VeList valueExpressions ->
                Guarded
                    { Guard = guard
                      Effect =
                        List.foldBack
                            (fun e a ->
                                match e with
                                | Infix(VPName accessor, Eq, rhs) -> (accessor, rhs) :: a
                                | _ ->
                                    failwith
                                        "Effect in transition rule must be infix expression with lhs being a primed reference")
                            valueExpressions
                            [] }
        
            | _ -> failwith "Effect in transition rule must be infix expression with lhs being a primed reference"
        | _ ->
            failwith
                "Transition rules must either be a guarded expression and a quantified expression of a guarded expression"
    | _ -> failwith "Not allowed"


let rec convertValueExpressionToIrTr valueExpr =
    match valueExpr with
    | Infix(lhs, InfixOp.Deterministic, rhs) ->
        Chain ((convertValueExpressionToIrRule rhs), Deterministic, (convertValueExpressionToIrTr lhs))
    | Infix(lhs, InfixOp.NonDeterministic, rhs) ->
        Chain ((convertValueExpressionToIrRule rhs), NonDeterministic, (convertValueExpressionToIrTr lhs))
    | _ -> Single (convertValueExpressionToIrRule valueExpr)

let convertTransitionRuleToIr valueExpr namedRules =
    { Rule = convertValueExpressionToIrTr valueExpr
      NamedRules =
        List.foldBack
            (fun ((id, _pos), valueExpr) a -> a.Add(id, convertValueExpressionToIrTr valueExpr))
            namedRules
            Map.empty }

let rec convertTransitionSystemToIr (id, trl) =
    let transitionSystemFolder dec ir =
        match dec with
        | Variable l ->
            { ir with
                Variable = Some(convertValueDeclToIr ir.Variable l) }
        | InitConstraint l ->
            { ir with
                InitConstraint = Some(convertAxiomDeclToIr l) }
        | TransitionRule(valueExpr, namedRules) ->
            { ir with TransitionRule = Some(convertTransitionRuleToIr valueExpr namedRules) }

    let initial =
        { Name = fst id
          Variable = None
          InitConstraint = None
          TransitionRule = None }
    
    List.foldBack transitionSystemFolder trl initial

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
                
                { intermediate with Value = Some(value) }
            | TypeDeclaration _ as td -> { intermediate with Type = Some(td) }
            | AxiomDeclaration ad ->
                { intermediate with
                    Axiom = Some(convertAxiomDeclToIr ad) }
            | TransitionSystemDeclaration(idPos, transitionSystems) ->
                { intermediate with TransitionSystem = Some(convertTransitionSystemToIr (idPos, transitionSystems)) }

        convertToIntermediate decls intermediate'

let rec axiomIrToAst (a: IrAxiomDeclaration) =
    match a with
    | IrQuantified(typings, irAxiomDeclaration) -> Quantified(All, typings, axiomIrToAst irAxiomDeclaration)
    | IrInfix(accessor, valueExpression) -> Infix(VName accessor, Equal, valueExpression)

/// <summary>
/// Convert <see cref="IrTransitionSystem"/> to <see cref="Declaration"/>
/// </summary>
/// <param name="inp"></param>
let transitionSystemDec (inp: Option<IrTransitionSystem>) : Option<Declaration> =
    let convertVariable var acc =
        match var with
        | None -> acc
        | Some m -> Variable(Map.foldBack (fun _k e a -> e :: a) m []) :: acc

    let convertInitConstraint ic acc =
        match ic with
        | None -> acc
        | Some l -> InitConstraint(List.foldBack (fun e a -> (axiomIrToAst e) :: a) l []) :: acc

    let IrGcToValueExpression (irGc: IrGc) =
        match irGc with
        | { Guard = guard; Effect = [acc, valueExpr] } -> Infix(guard, Guard, Infix(VPName acc, Equal, valueExpr))
        | { Guard = guard; Effect = tuples } ->
            Infix(guard, Guard, VeList (List.foldBack (fun (acc, valueExpr) a -> Infix(VPName acc, Equal, valueExpr)::a) tuples []))
            
    let IrRuleToValueExpression (irRule: IrRule) =
        match irRule with
        | Guarded irGc -> failwith "todo"
        | Quan foo -> failwith "todo"
    
    let rec IrTrToValueExpression (irTr: IrTr) =
        match irTr with
        | Single irRule -> failwith "todo"
        | Chain(irRule, choice, irTr) ->
            match choice with
            | NonDeterministic ->
                Infix(IrRuleToValueExpression irRule, InfixOp.NonDeterministic, IrTrToValueExpression irTr)
            | Deterministic ->
                Infix(IrRuleToValueExpression irRule, InfixOp.Deterministic, IrTrToValueExpression irTr)
    
    let convertTransitionRule (tr: Option<IrTransitionRule>) acc =
        match tr with
        | None -> acc
        | Some r ->
            TransitionRule(IrTrToValueExpression r.Rule, Map.foldBack (fun k e a -> ((k, dp), IrTrToValueExpression e) :: a) r.NamedRules [])
            :: acc

    match inp with
    | None -> None
    | Some ts ->
        Some(
            TransitionSystemDeclaration(
                (ts.Name, dp),
                (convertTransitionRule ts.TransitionRule []
                 |> convertInitConstraint ts.InitConstraint)
                |> convertVariable ts.Variable
            )
        )

/// <summary>
/// Convert Intermediate representation back to AST.
///
/// This is used when writing the specification to any source language
/// </summary>
/// <param name="intermediate"></param>
let rec convertToAst (intermediate: Intermediate) =
    let trDec = transitionSystemDec intermediate.TransitionSystem

    let axiomDec =
        match intermediate.Axiom with
        | None -> None
        | Some v -> Some(AxiomDeclaration(List.foldBack (fun e a -> (axiomIrToAst e) :: a) v []))

    let valueDec =
        match intermediate.Value with
        | Some v when Map.empty <> v  ->
            let value = Map.foldBack (fun _ v a -> v :: a) v []
            Some (Value value)
        | _ -> None

    let typeDec =
        match intermediate.Type with
        | None -> None
        | Some v -> Some(v)
        
    typeDec :: valueDec :: axiomDec :: [trDec] |> List.choose id 

/// <summary>
/// Build symbol table for given Abstract Syntax Tree
/// </summary>
/// <param name="_AST"></param>
let buildSymbolTable (_AST: Class) =

    let unfoldTypeEnvironments acc dec =
        match dec with
        | Value _ -> acc
        | TypeDeclaration ts -> ts @ acc
        | AxiomDeclaration _ -> acc
        | TransitionSystemDeclaration _ -> acc

    let buildType (env: Map<string, TypeDefinition>) =
        function
        | id, typeDecl -> env.Add((fst id), typeDecl)

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

    let unfoldValueValues (map: Map<string, TypeExpression>) valueExpr =
        match valueExpr with 
        | ExplicitValue(s, typeExpression, _) ->
            match s with
            | ISimple s' -> map.Add((fst s'), typeExpression)
            | IGeneric _ -> failwith "todo"
            
        | GenericValue(s, _, typeExpression) -> 
            match s with
            | ISimple s' -> map.Add((fst s'), typeExpression)
            | IGeneric _ -> failwith "todo"
        | Typing(SingleTyping(id, typeExpr)) ->
            match id with
            | ISimple(id', _)
            | IGeneric((id', _), _) -> map.Add(id', typeExpr)
        | _ -> map

    List.fold unfoldValueEnvironments [] _AST |> List.fold unfoldValueValues Map.empty
