module AstDrawerLibrary.AstDrawer

open Transpiler
open Transpiler.Ast
open AstDrawerLibrary.Library
open Transpiler.Helpers.Helpers

let typeDeclarationToNode (typeDeclaration: Pos<Id> * TypeDefinition) (acc: Tree<string> list) : Tree<string> list =
    match typeDeclaration with
    | (s, _pos), Abstract -> Node($"{s}", [ Node("Abstract", []) ]) :: acc
    | (s, _pos), Concrete _ -> Node($"{s}", [ Node("Concrete", [ Node("<type_expr>", []) ]) ]) :: acc
    | (s, _pos), Union tuples ->
        Node($"{s}", [ Node("Variant", List.foldBack (fun (i, _pos) a -> Node(i, []) :: a) tuples []) ])
        :: acc


let infixOpToNode (infixOp: InfixOp) =
    match infixOp with
    | Equal -> Node("Equal", [])
    | NotEqual -> Node("NotEqual", [])
    | Plus -> Node("Plus", [])
    | Minus -> Node("Minus", [])
    | Guard -> Node("Guard", [])
    | Deterministic -> Node("[>]", [])
    | NonDeterministic -> Node("[=]", [])
    | LessThan -> Node("<", [])
    | LessThanOrEqual -> Node("<=", [])
    | GreaterThan -> Node(">", [])
    | GreaterThanOrEqual -> Node(">=", [])
    | Implies -> Node("Implies", [])
    | LogicalAnd -> Node("And", [])
    | LogicalOr -> Node("Or", [])

let temporalModalOperatorsToString (tmo: TemporalModalOperators) : string =
    match tmo with
    | Globally -> "G"
    | Finally -> "F"
    | Release -> "R"
    | Weak -> "W"
    | Mighty -> "M"

let rec valueExpressionToNode (valueExpression: ValueExpression) (acc: Tree<string> list) : Tree<string> list =
    match valueExpression with
    | ValueLiteral(valueLiteral, _pos) -> Node("Literal", [ Node(literalToString valueLiteral, []) ]) :: acc
    | VName accessor -> Node("Name", [ accessorToNode accessor ]) :: acc
    | VPName accessor -> Node("PName", [ accessorToNode accessor ]) :: acc
    | Rule(id, _pos) -> Node("Rule", [ Node(id, []) ]) :: acc
    | Quantified((quantifier, _pos), typings, valueExpression) ->
        Node($"{quantifier}", (valueExpressionToNode valueExpression []) |> List.foldBack typingToNode typings)
        :: acc
    | Infix(lhs, infixOp, rhs) ->
        Node(
            "Infix",
            (infixOpToNode infixOp :: valueExpressionToNode rhs []
             |> valueExpressionToNode lhs)
        )
        :: acc
    | VeList valueExpressions -> Node("VeList", List.foldBack valueExpressionToNode valueExpressions []) :: acc
    | VArray valueExpressions -> Node("VArray", List.foldBack valueExpressionToNode valueExpressions []) :: acc
    | LogicalNegation(valueExpression, _pos) -> Node("Negation", valueExpressionToNode valueExpression []) :: acc
    | Prefix((tmo, _pos), valueExpression) ->
        Node(temporalModalOperatorsToString tmo, valueExpressionToNode valueExpression [])
        :: acc

and accessorToNode (accessor: Accessor) : Tree<string> =
    match accessor with
    | ASimple(s, _pos) -> Node("Simple", [ Node(s, []) ])
    | AGeneric((s, _pos), valueExprs) ->
        Node("Generic", Node(s, []) :: List.foldBack valueExpressionToNode valueExprs [])

and identifierToNode (identifier: Identifier) : Tree<string> =
    match identifier with
    | ISimple(s, _pos) -> Node("Simple", [ Node(s, []) ])
    | IGeneric((s, _pos), typings) -> Node("Generic", Node(s, []) :: List.foldBack typingToNode typings [])

and typeExpressionToNode (typeExpression: TypeExpression) (acc: Tree<string> list) : Tree<string> list =
    match typeExpression with
    | Literal typeLiteral -> Node(typeLiteral.ToString(), []) :: acc
    | TName(name, _pos) -> Node(name, []) :: acc
    | Product typeExpressions -> Node("Product", List.foldBack typeExpressionToNode typeExpressions []) :: acc
    | Set typeExpression -> Node("Set", typeExpressionToNode typeExpression []) :: acc
    | List typeExpression -> Node("List", typeExpressionToNode typeExpression []) :: acc
    | Map(typeExpression, expression) ->
        Node("Map", typeExpressionToNode typeExpression [] |> typeExpressionToNode expression)
        :: acc
    | TArray(typeExpression, expression) ->
        Node("TArray", typeExpressionToNode typeExpression [] |> typeExpressionToNode expression)
        :: acc
    | Sub(typings, valueExpression) ->
        Node("Sub", valueExpressionToNode valueExpression [] |> List.foldBack typingToNode typings)
        :: acc

and typingToNode (typing: Typing) (acc: Tree<string> list) : Tree<string> list =
    match typing with
    | SingleTyping(identifier, typeExpression) ->
        Node("Typing", [ identifierToNode identifier ] @ typeExpressionToNode typeExpression [])
        :: acc

let valueDeclarationToNode (valueDeclaration: ValueDeclaration) (acc: Tree<string> list) : Tree<string> list =
    match valueDeclaration with
    | ExplicitValue(identifier, typeExpression, valueExpression) ->
        Node(
            "ExplicitValue",
            [ identifierToNode identifier ]
            @ typeExpressionToNode typeExpression []
            @ valueExpressionToNode valueExpression []
        )
        :: acc
    | Typing(SingleTyping _ as t) -> typingToNode t [] @ acc

let transitionSystemToNode (transitionSystem: TransitionSystem) (acc: Tree<string> list) : Tree<string> list =
    match transitionSystem with
    | Variable valueDeclarations -> Node("Variable", List.foldBack valueDeclarationToNode valueDeclarations [])
    | InitConstraint valueExpression -> Node("InitConstraint", valueExpressionToNode valueExpression [])
    | TransitionRule(valueExpression, tuples) ->
        Node(
            "TransitionRule",
            valueExpressionToNode valueExpression []
            @ List.foldBack
                (fun ((name, _pos), valueExpression) a -> Node(name, valueExpressionToNode valueExpression []) :: a)
                tuples
                []
        )
    :: acc

let ltlAssertionToNode ((name, ts, valueExpr): LtlAssertion) (acc: Tree<string> list) : Tree<string> list =
    Node(fst name, [ Node(fst ts, valueExpressionToNode valueExpr []) ]) :: acc


let declarationToNode (declaration: Declaration) (acc: Tree<string> list) : Tree<string> list =
    match declaration with
    | Value valueDeclarations -> Node("Value", List.foldBack valueDeclarationToNode valueDeclarations []) :: acc
    | TypeDeclaration tuples -> Node("Type", List.foldBack typeDeclarationToNode tuples []) :: acc
    | AxiomDeclaration valueExpressions -> Node("Axiom", List.foldBack valueExpressionToNode valueExpressions []) :: acc
    | TransitionSystemDeclaration((id, _pos), transitionSystems) ->
        Node($"TransitionSystem {id}", List.foldBack transitionSystemToNode transitionSystems [])
        :: acc
    | LtlAssertionDeclaration ltls -> Node("LTL", List.foldBack ltlAssertionToNode ltls []) :: acc


let schemeToTree ((_s, _), _ as scheme: Scheme) (name: string) : Tree<string> =

    let tree =
        match scheme with
        | (s, _pos), declarations -> Node($"Scheme {s}", List.foldBack declarationToNode declarations [])

    treeToFile name tree

    tree
