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
    | VUnit of Unit
    | VBool of bool
    | VInt of int
    | VReal of int
    | VChar of char
    | VNat of int
    | VText of string

type TypeExpression = 
    | Literal of TypeLiteral
    | Name of Id
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

type ValueExpression =
    | ValueLiteral of ValueLiteral

// AST Node
type ValueDeclaration =
    | ExplicitValue of (Id * TypeExpression * ValueExpression)
    | ImplicitValue
    | ExplicitFunction
    | ImplicitFunction
    | GenericValue of (Id * Typing list * TypeExpression)
    | Typing of Typing

type Accessor =
    | Id of Id
    | Generic of (Id * ValueExpression list)

type Axiom =
    | Axiom of (Accessor * ValueExpression)
    | Forall of (Typing list * Axiom)

type Declaration =
    | Value of ValueDeclaration list
    | TypeDeclaration of (Id * TypeDefinition) list
    | AxiomDeclaration of Axiom list
    
type Class = Declaration list

type Scheme = Id * Class
