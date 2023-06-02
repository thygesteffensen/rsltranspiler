/// <summary>
/// Module to convert from AST to IR and from IR to AST.
///
/// The conversions are simple are are defined as pairs, i.e. the function converting one node in the AST to IR is
/// placed together with IR to AST functions. The process is the same, but reversed and this maintain keeping a better
/// overview.
/// </summary>
module Transpiler.Helpers.IrTransformer

open FSharp.Text.Lexing
open Microsoft.FSharp.Collections
open Transpiler.Auxiliary
open Transpiler.Intermediate
open Transpiler.Ast

let dummyPos: Position =
    { pos_bol = -1
      pos_cnum = -1
      pos_lnum = -1
      pos_orig_lnum = -1
      pos_fname = "None" }

let idL =
    function
    | None -> []
    | Some v -> v

let idM =
    function
    | None -> Map.empty
    | Some v -> v


/// <summary>
/// Folder for TransitionSystem lists to convert TransitionSystem to IrTransitionSystem
/// </summary>
/// <param name="tr"></param>
/// <param name="acc"></param>
let transitionSystemFolder (tr: TransitionSystem) (acc: IrTransitionSystem) : IrTransitionSystem =
    match tr with
    | Variable valueDeclarations ->
        let name (id: Identifier) : string =
            match id with
            | ISimple(id, _) -> id
            | IGeneric((id, _), _) -> id

        let folder (valueDec: ValueDeclaration) (acc: IrTransitionSystem) =
            match valueDec with
            | ImplicitValue -> failwith "todo"
            | ExplicitFunction -> failwith "todo"
            | ImplicitFunction -> failwith "todo"
            | ExplicitValue(id, _, _)
            | GenericValue(id, _, _)
            | Typing(SingleTyping(id, _)) ->
                { acc with
                    Variable = Some((idM acc.Variable).Add(name id, valueDec)) }

        List.foldBack folder valueDeclarations acc
    | InitConstraint valueExpressions ->
        // Init Constraint must be assignment expression
        // <name> ":=" <value_expr>
        // or an all quantification of this
        let folder (valueExpr: ValueExpression) (acc: IrTransitionSystem) =
            match valueExpr with
            | Infix(VName s, Equal, expression) ->
                { acc with
                    InitConstraint = Some(IrInfix(s, expression) :: idL acc.InitConstraint) }

            | ValueExpression.Quantified((Quantifier.All, _), typings, Infix(VName s, Equal, expression)) ->
                { acc with
                    InitConstraint = Some(IrQuantified(typings, IrInfix(s, expression)) :: idL acc.InitConstraint) }
            | _ -> failwith "No allowed as init constraint"

        List.foldBack folder valueExpressions acc
    | TransitionRule(valueExpr, tuples) ->
        let rec valueExprToTransitionRule (valueExpr: ValueExpression) : IrTransitionRule =
            match valueExpr with
            | VName(ASimple(id, _)) -> Name id // TODO: LookUp name
            | VName(AGeneric((_, pos), _)) -> failwith $"Value Name at {pos} must be a Named Transition Rule"

            | ValueExpression.Quantified((Quantifier.NonDeterministic, _), typings, valueExprs) ->
                IrTransitionRule.Quantified(Choice.NonDeterministic, typings, valueExprToTransitionRule valueExprs)
            | ValueExpression.Quantified _ ->
                failwith $"Quantified transition rule at {getPosFromValueExpression valueExpr} must be the non deterministic choice"

            | Infix(guard, InfixOp.Guard, VeList effects) ->
                let folder (valueExpr: ValueExpression) (acc: Effect list) : Effect list =
                    match valueExpr with
                    | Infix(VPName accessor, Equal, expression) -> (accessor, expression) :: acc
                    | _ -> failwith $"Guarded Expression effect at {getPosFromValueExpression (VeList effects)}  must be a list of prime value updates"

                Guarded(guard, (List.foldBack folder effects []))
            | _ -> failwith $"Transition rule at {getPosFromValueExpression valueExpr} must be a guarded command." 

        let rec valueExprToTransitionRules (valueExpr: ValueExpression) : IrTransitionRules =
            match valueExpr with
            | Infix(lhs, InfixOp.NonDeterministic, rhs) ->
                Node(valueExprToTransitionRules lhs, Choice.NonDeterministic, valueExprToTransitionRules rhs)
            | _ -> valueExprToTransitionRule valueExpr |> Leaf

        let folder (((id, _), valueExpr): Pos<Id> * ValueExpression) acc : Map<string, IrTransitionRules> =
            Map.add id (valueExprToTransitionRules valueExpr) acc

        { acc with
            TransitionRule = Some(valueExprToTransitionRules valueExpr, List.foldBack folder tuples Map.empty) }

/// <summary>
/// Convert Transition System from AST to Intermediate Representation
/// </summary>
/// <param name="id"></param>
/// <param name="trs"></param>
let convertAstTransitionSystemToIr (id: Pos<Id>) (trs: TransitionSystem list) : IrTransitionSystem =
    let initial =
        { Name = fst id
          Variable = None
          InitConstraint = None
          TransitionRule = None }

    List.foldBack transitionSystemFolder trs initial

/// <summary>
/// Convert Transition System from Intermediate Representation to Ir
/// </summary>
/// <param name="irTransitionSystem"></param>
let convertIrTransitionSystemToAst (irTransitionSystem: IrTransitionSystem) : Declaration =
    let variableDeclaration =
        match irTransitionSystem.Variable with
        | None -> None
        | Some value ->
            let folder (_k: string) (e: ValueDeclaration) (a: ValueDeclaration list) = e :: a
            Some(Variable(Map.foldBack folder value []))

    let initConstraintDeclaration =
        match irTransitionSystem.InitConstraint with
        | None -> None
        | Some value ->
            let rec folder (e: IrAxiomDeclaration) (acc: ValueExpression list) : ValueExpression list =
                match e with
                | IrQuantified(typings, IrInfix(accessor, valueExpression)) ->
                    ValueExpression.Quantified((All, dummyPos), typings, Infix(VName accessor, Equal, valueExpression))
                    :: acc
                | IrInfix(accessor, valueExpression) -> Infix(VName accessor, Equal, valueExpression) :: acc
                | _ -> failwith "Exception: Other construct should not be encountered here."

            Some(InitConstraint(List.foldBack folder value []))

    let transitionSystemDeclaration =
        let rec irTransitionRuleToAst (irTransitionRule: IrTransitionRule) : ValueExpression =
            match irTransitionRule with
            | Guarded(guard, effects) ->
                Infix(
                    guard,
                    Guard,
                    VeList(
                        List.foldBack
                            (fun (accessor, valueExpr) a -> Infix(VPName accessor, Equal, valueExpr) :: a)
                            effects
                            []
                    )
                )
            | Name s -> VPName(ASimple((s, dummyPos)))
            | IrTransitionRule.Quantified(Choice.NonDeterministic, typings, irTransitionRule) ->
                ValueExpression.Quantified((Quantifier.NonDeterministic, dummyPos), typings, irTransitionRuleToAst irTransitionRule)
            | _ -> failwith "Not possible"

        let rec irTransitionRulesToAst (ir: IrTransitionRules) : ValueExpression =
            match ir with
            | Node(lhs, Choice.NonDeterministic, rhs) ->
                Infix(irTransitionRulesToAst lhs, InfixOp.NonDeterministic, irTransitionRulesToAst rhs)
            | Leaf irTransitionRule -> irTransitionRuleToAst irTransitionRule
            | _ -> failwith "Not possible"

        match irTransitionSystem.TransitionRule with
        | None -> None
        | Some(irTransitionRules, irTransitionRulesMap) ->
            Some(
                TransitionRule(
                    irTransitionRulesToAst irTransitionRules,
                    Map.foldBack (fun k e a -> ((k, dummyPos), (irTransitionRulesToAst e)) :: a) irTransitionRulesMap []
                )
            )

    TransitionSystemDeclaration(
        (irTransitionSystem.Name, dummyPos),
        List.choose
            id
            (variableDeclaration
             :: (initConstraintDeclaration :: [ transitionSystemDeclaration ]))
    )
