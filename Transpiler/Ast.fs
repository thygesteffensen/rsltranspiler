namespace Transpiler

open Microsoft.FSharp.Core

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
    | TName of Id
    | Product of TypeExpression list
    | Set of TypeExpression
    | List of TypeExpression
    | Map of (TypeExpression * TypeExpression)
    // | Function
    // | Subtype
    // | Bracketed // Necessary?

// AST Node
type TypeDefinition =
    | Abstract // Sort
    | Concrete of TypeExpression // Abbreviation t = Nat
    | Union of Id list
    // | Typing of TypeExpression // t : Nat

type Typing =
    | SingleTyping of Id * TypeExpression 

type Quantifier = All | Exists | ExactlyOne

type ValueExpression =
    | ValueLiteral of ValueLiteral
    | VName of Id
    | GenericName of (Id * ValueExpression list)
    | Equivalence of (ValueExpression * ValueExpression)
    | Quantified of (Quantifier * Typing list * ValueExpression)


// AST Node
type ValueDeclaration =
    | ExplicitValue of (Id * TypeExpression * ValueExpression)
    | ImplicitValue
    | ExplicitFunction
    | ImplicitFunction
    | GenericValue of (Id * Typing list * TypeExpression)
    | Typing of Typing

type Declaration =
    | Value of ValueDeclaration list
    | TypeDeclaration of (Id * TypeDefinition) list
    | AxiomDeclaration of ValueExpression list
    
type Class = Declaration list

type Scheme = Id * Class
