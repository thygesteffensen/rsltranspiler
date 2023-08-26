module Transpiler.RuleCollection.GenericValueDefinitionRule

open Transpiler.Ast
open Transpiler.Intermediate
open Transpiler.Helpers.Helpers
open Transpiler.RuleCollection.QuantificationRule


let extractIds (typings: Typing list) : Id list =
    List.foldBack
        (fun (SingleTyping(identifier, _)) a ->
            match identifier with
            | ISimple(id, _pos) -> id :: a
            | IGeneric((_, pos), _) -> failWithLine pos "Typing in ExplicitValue must be simple.")
        typings
        []

let valueDeclarationFolder
    (typeEnv: TypeEnvMap)
    (valueEnv: ValueEnvMap)
    (valueDecl: ValueDeclaration)
    (a: ValueDeclaration list)
    : ValueDeclaration list =
    match valueDecl with
    | Typing(SingleTyping(ISimple _, _)) -> [ valueDecl ]
    | ExplicitValue(ISimple _, _, _) -> [ valueDecl ]
    | Typing(SingleTyping(IGeneric(idPos, typings), typeExpr)) ->

        let l =
            genericInstantiateTypings typeEnv Map.empty valueEnv typings [] () (fun _ _ valueEnv acc _ ->
                let simpleId =
                    List.fold
                        (fun a e ->
                            match Map.tryFind e valueEnv with
                            | None -> failwith $"ValueName {e} not found in the value env"
                            | Some value -> a + "_" + (literalToString value))
                        (fst idPos)
                        (extractIds typings)

                Typing(SingleTyping(ISimple((simpleId, snd idPos)), typeExpr)) :: acc)

        l
    | ExplicitValue(IGeneric(idPos, typings), typeExpr, valueExpr) ->
        let l =
            genericInstantiateTypings typeEnv Map.empty valueEnv typings [] valueExpr (fun typeEnv _ valueEnv acc e ->
                let simpleId =
                    List.fold
                        (fun a e ->
                            match Map.tryFind e valueEnv with
                            | None -> failwith $"ValueName {e} not found in the value env"
                            | Some value -> a + "_" + (literalToString value))
                        (fst idPos)
                        (extractIds typings)

                ExplicitValue(ISimple((simpleId, snd idPos)), typeExpr, unfoldQuantified typeEnv valueEnv e)
                :: acc)

        l
    @ a


let transitionSystemFolder
    (typeEnv: TypeEnvMap)
    (valueEnv: ValueEnvMap)
    (transitionSystem: TransitionSystem)
    (a: TransitionSystem list)
    : TransitionSystem list =
    match transitionSystem with
    | Variable valueDeclarations ->
        List.foldBack (valueDeclarationFolder typeEnv valueEnv) valueDeclarations []
        |> Variable
    | InitConstraint _ -> transitionSystem
    | TransitionRule _ -> transitionSystem
    :: a

let classFolder (typeEnv: TypeEnvMap) (valueEnv: ValueEnvMap) (e: Declaration) (a: Class) : Class =
    match e with
    | Value valueDeclarations ->
        List.foldBack (valueDeclarationFolder typeEnv valueEnv) valueDeclarations []
        |> Value
    | TypeDeclaration _ -> e
    | AxiomDeclaration _ -> e
    | TransitionSystemDeclaration(idPos, transitionSystems) ->
        TransitionSystemDeclaration(idPos, List.foldBack (transitionSystemFolder typeEnv valueEnv) transitionSystems [])
    | LtlAssertionDeclaration _ -> e
    :: a

let unfoldGenericValueDeclarations
    (typeEnv: TypeEnvMap)
    (valueEnv: ValueEnvMap)
    (cls: Class)
    : Class =
    List.foldBack (classFolder typeEnv valueEnv) cls []
