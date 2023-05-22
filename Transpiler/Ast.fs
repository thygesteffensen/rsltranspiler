namespace Transpiler

open FSharp.Text.Lexing

type Pos<'a> = 'a * Position

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

type TypeExpression =
    | Literal of TypeLiteral
    | TName of Pos<Id>
    | Product of TypeExpression list
    | Set of TypeExpression
    | List of TypeExpression
    | Map of (TypeExpression * TypeExpression)
// | Function
// | Subtype

// AST Node
type TypeDefinition =
    | Abstract // Sort
    | Concrete of TypeExpression // Abbreviation t = Nat
    | Union of Pos<Id> list

type Identifier =
    | ISimple of Pos<Id>
    | IGeneric of (Pos<Id> * Typing list)

and Typing = SingleTyping of Identifier * TypeExpression

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

type ValueExpression =
    | ValueLiteral of Pos<ValueLiteral>
    | VName of Accessor
    | VPName of Accessor
    // | GenericName of (Id * ValueExpression list)
    // | PGenericName of (Id * ValueExpression list)
    | Quantified of (Quantifier * Typing list * ValueExpression)
    | Infix of (ValueExpression * InfixOp * ValueExpression)
    
and Accessor =
    | ASimple of Pos<Id>
    | AGeneric of (Pos<Id> * ValueExpression list)

// AST Node
type ValueDeclaration =
    | ExplicitValue of (Identifier * TypeExpression * ValueExpression)
    | ImplicitValue
    | ExplicitFunction
    | ImplicitFunction
    | GenericValue of (Identifier * Typing list * TypeExpression)
    | Typing of Typing


type TransitionSystem =
    | Variable of ValueDeclaration list
    | InitConstraint of ValueExpression list
    | TransitionRule of ValueExpression * (Pos<Id> * ValueExpression) list

type Declaration =
    | Value of ValueDeclaration list
    | TypeDeclaration of (Pos<Id> * TypeDefinition) list
    | AxiomDeclaration of ValueExpression list
    | TransitionSystemDeclaration of (Pos<Id> * TransitionSystem list)

type Class = Declaration list

type Scheme = Pos<Id> * Class

