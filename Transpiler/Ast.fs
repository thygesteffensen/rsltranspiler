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
    
type TypeDeclaration =
    | Abstract of Id // Sort
    | Concrete of (Id * TypeExpression) // Abbreviation t = Nat
    | Union of (Id * Id list)
    | Typing of (Id * TypeExpression) // t : Nat


type ValueExpression =
    | ValueLiteral of ValueLiteral

type ValueDeclaration =
    | ExplicitValue of (Id * TypeExpression * Option<ValueExpression>)
    | ImplicitValue
    | ExplicitFunction
    | ImplicitFunction
    | GenericValue of (Id * TypeDeclaration list * TypeExpression)

type Declaration =
    | Value of ValueDeclaration list
    | Type of TypeDeclaration list

type Class = Declaration list

type Scheme = Id * Class
