namespace Transpiler

type Id = string



type Literal =
    | Unit
    | Bool
    | Int
    | Real
    | Char
    | Nat
    | Text

type TypeExpression =
    | Literal of Literal
    | Name of Id
    | Product of TypeExpression list
    | Set of TypeExpression
    | List of TypeExpression
    | Map of (TypeExpression * TypeExpression)
    // | Function
    // | Subtype
    // | Bracketed // Necessary?
type TypeDeclaration =
    | Abstract of Id // Abbreviation
    | Concrete of (Id * TypeExpression)
    | Union of (Id * Id list)

type ValueDeclaration = Id list

type Declaration =
    | Value of ValueDeclaration
    | Type of TypeDeclaration list

type Class = Declaration

type Scheme = Id * Class
