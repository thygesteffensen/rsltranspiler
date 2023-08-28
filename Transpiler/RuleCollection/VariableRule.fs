module Transpiler.RuleCollection.VariableRule

open Transpiler.Ast
open Transpiler.Auxiliary
open Transpiler.Helpers

let valueDeclarationFolder
    (valueDecl: ValueDeclaration)
    ((a1, a2): ValueDeclaration list * ValueExpression list)
    : ValueDeclaration list * ValueExpression list =
    match valueDecl with
    | Typing _ -> (valueDecl :: a1, a2)
    | ExplicitValue(IGeneric((_, pos), _), _, _) -> failWithLine pos "Generic explicit value not allowed at this stage"
    | ExplicitValue(ISimple(idPos) as i, t, v) ->
        let a1' = Typing(SingleTyping(i, t))
        let a2' = Infix(VName(ASimple(idPos)), Equal, v)
        (a1' :: a1, a2' :: a2) // TODO Lav tests for dette

let transitionSystemFolder
    (transitionSystem: TransitionSystem)
    ((a1, a2): TransitionSystem list * ValueExpression list)
    : TransitionSystem list * ValueExpression list =
    match transitionSystem with
    | Variable valueDeclarations ->
        let implicitVariables, initConstraintAddition =
            List.foldBack valueDeclarationFolder valueDeclarations ([], [])

        ((implicitVariables |> Variable) :: a1, initConstraintAddition)
    | InitConstraint _ -> (transitionSystem :: a1, a2)
    | TransitionRule _ -> (transitionSystem :: a1, a2)

let classFolder (decl: Declaration) ((a1, a2): Class * ValueExpression list) : Class * ValueExpression list =
    match decl with
    | TransitionSystemDeclaration(idPos, transitionSystems) ->
        let a1', a2' = List.foldBack transitionSystemFolder transitionSystems ([], [])

        ((TransitionSystemDeclaration(idPos, a1') :: a1), a2' @ a2)

    | _ -> (decl :: a1, a2)

/// <summary>
/// This will add the collected init constraint to all init constraints if there is multiple. This is not allowed
/// </summary>
/// <param name="inits"></param>
/// <param name="decl"></param>
/// <param name="acc"></param>
let combineInitConstraintsFolder (inits: ValueExpression list) (decl: Declaration) (acc: Class) : Class =
    match decl with
    | AxiomDeclaration valueExpressions -> AxiomDeclaration(valueExpressions @ inits) :: acc
    | _ -> decl :: acc

let makeVariableDeclarationImplicit
    (_typeEnv: TypeEnvMap)
    (_valueEnv: ValueEnvMap)
    (cls: Class)
    : TypeEnvMap * ValueEnvMap * Class =
    // 1. Gather all init constraints
    // 2. Add them to the init constraint and maybe level them out a bit
    let cls', inits = List.foldBack classFolder cls ([], [])

    let cls' = List.foldBack (combineInitConstraintsFolder inits) cls' []

    (_typeEnv, _valueEnv, cls')
