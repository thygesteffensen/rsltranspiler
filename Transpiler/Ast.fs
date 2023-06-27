namespace Transpiler.Ast

open FSharp.Text.Lexing

type Pos<'a> =
    'a * Position

type TokenPosition = { Line: int; Column: int }

type Id = string

type TypeLiteral =
    | TUnit
    | TBool
    | TInt
    | TReal
    | TChar
    | TNat
    | TText

type ValueLiteral =
    | VUnit of unit
    | VBool of bool
    | VInt of int
    | VReal of int
    | VChar of char
    | VNat of int
    | VText of string

type Quantifier =
    | All
    | Exists
    | ExactlyOne
    | Deterministic
    | NonDeterministic

type InfixOp =
    | Equal
    | Plus
    | Guard
    | Deterministic
    | NonDeterministic
    | LessThan
    | LessThanOrEqual
    | GreaterThan
    | GreaterThanOrEqual
    | Implies
    | LogicalAnd
    | LogicalOr

type ValueExpression =
    | ValueLiteral of Pos<ValueLiteral>
    | VName of Accessor
    | VPName of Accessor
    | Rule of Pos<Id>
    | Quantified of (Pos<Quantifier> * Typing list * ValueExpression)
    | Infix of (ValueExpression * InfixOp * ValueExpression)
    | VeList of ValueExpression list
    | VArray of ValueExpression list

    (*override this.ToString() =
        "This is the way"
        (*match this with
        | ValueLiteral(valueLiteral, _) -> $"ValueLiteral({valueLiteral.ToString()})"
        | VName accessor -> $"VName({accessor.ToString()})"
        | VPName accessor -> $"VPName({accessor.ToString()})"
        | Rule tuple -> $"Rule({tuple.ToString()})"
        | Quantified foo -> $"Quantified({foo.ToString()})"
        | Infix foo -> $"Infix({foo.ToString()})"
        | VeList valueExpressions -> $"VeList({valueExpressions.ToString()})"
        | VArray valueExpressions -> $"VArray({valueExpressions.ToString()})"*)*)

and Accessor =
    | ASimple of Pos<Id>
    | AGeneric of (Pos<Id> * ValueExpression list)


and Identifier =
    | ISimple of Pos<Id>
    | IGeneric of (Pos<Id> * Typing list)
    
    (*override this.ToString() =
        match this with
        | ISimple (s, _) -> $"ISimple({s})"
        | IGeneric((s, _), o) -> $"IGeneric({(s, o)})"*) 

and Typing = SingleTyping of Identifier * TypeExpression
    

and TypeExpression =
    | Literal of TypeLiteral
    | TName of Pos<Id>
    | Product of TypeExpression list
    | Set of TypeExpression
    | List of TypeExpression
    | Map of (TypeExpression * TypeExpression)
    | TArray of (TypeExpression * TypeExpression)
    | Sub of (Typing list * ValueExpression)

// AST Node
type TypeDefinition =
    | Abstract // Sort
    | Concrete of TypeExpression // Abbreviation t = Nat
    | Union of Pos<Id> list
    
    (*override this.ToString() =
        match this with
        | Abstract -> "Abstract()"
        | Concrete typeExpression -> failwith "todo"
        | Union tuples ->
            let sep = ", "
            $"Union ([{String.concat sep (List.map (fun (e, _) -> e.ToString()) tuples) }])"*)


// AST Node
type ValueDeclaration =
    | ExplicitValue of (Identifier * TypeExpression * ValueExpression)
    | ImplicitValue
    | ExplicitFunction
    | ImplicitFunction
    | GenericValue of (Identifier * Typing list * TypeExpression)
    | Typing of Typing
    
    (*override this.ToString() =
        match this with
        | ExplicitValue(identifier, typeExpression, valueExpression) ->
            $"ExplicitValue({identifier}, {typeExpression}, {valueExpression}"
        | ImplicitValue -> "ImplicitValue"
        | ExplicitFunction -> "ExplicitFunction"
        | ImplicitFunction -> "ImplicitFunction"
        | GenericValue(identifier, typings, typeExpression) ->
            $"GenericValue({identifier}, {typings}, {typeExpression})"
        | Typing typing -> $"Typing({typing})"*)


type TransitionSystem =
    | Variable of ValueDeclaration list
    | InitConstraint of ValueExpression
    | TransitionRule of ValueExpression * (Pos<Id> * ValueExpression) list

type Declaration =
    | Value of ValueDeclaration list
    | TypeDeclaration of (Pos<Id> * TypeDefinition) list
    | AxiomDeclaration of ValueExpression list
    | TransitionSystemDeclaration of (Pos<Id> * TransitionSystem list)
    
    (*override this.ToString() =
        match this with
        | Value valueDeclarations -> $"Value({valueDeclarations.ToString()})"
        | TypeDeclaration tuples ->
            $"TypeDeclaration({List.map (fun ((i, _), e) -> (i, e)) tuples})"
        | AxiomDeclaration valueExpressions ->
            valueExpressions.ToString()
        | TransitionSystemDeclaration((s, _), transitionSystems) -> $"TransitionSystemDeclaration({s}, {transitionSystems.ToString()})"*)

type Class = Declaration list

type Scheme = Pos<Id> * Class

