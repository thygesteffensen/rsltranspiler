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
    | NotEqual
    | Plus
    | Minus
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

type TemporalModalOperators =
    | Globally
    | Finally
    | Release
    | Weak
    | Mighty

type ValueExpression =
    | ValueLiteral of Pos<ValueLiteral>
    | VName of Accessor
    | VPName of Accessor
    | Rule of Pos<Id>
    | Quantified of (Pos<Quantifier> * Typing list * ValueExpression)
    | Infix of (ValueExpression * InfixOp * ValueExpression)
    | VeList of ValueExpression list
    | VArray of ValueExpression list
    | LogicalNegation of Pos<ValueExpression>
    | Prefix of Pos<TemporalModalOperators> * ValueExpression

and Accessor =
    | ASimple of Pos<Id>
    | AGeneric of (Pos<Id> * ValueExpression list)

and Identifier =
    | ISimple of Pos<Id>
    | IGeneric of (Pos<Id> * Typing list) 

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

// AST Node
type ValueDeclaration =
    | ExplicitValue of (Identifier * TypeExpression * ValueExpression)
    | GenericValue of (Identifier * Typing list * TypeExpression)
    | Typing of Typing

type TransitionSystem =
    | Variable of ValueDeclaration list
    | InitConstraint of ValueExpression
    | TransitionRule of ValueExpression * (Pos<Id> * ValueExpression) list

type LtlAssertion = Pos<Id> * Pos<Id> * ValueExpression

type Declaration =
    | Value of ValueDeclaration list
    | TypeDeclaration of (Pos<Id> * TypeDefinition) list
    | AxiomDeclaration of ValueExpression list
    | TransitionSystemDeclaration of (Pos<Id> * TransitionSystem list)
    | LtlAssertionDeclaration of LtlAssertion list

type Class = Declaration list

type Scheme = Pos<Id> * Class
