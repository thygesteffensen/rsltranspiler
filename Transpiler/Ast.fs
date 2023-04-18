namespace Transpiler

type ValueDeclaration = string list

type TypeDeclaration = string list

type Declaration =
    | Value of ValueDeclaration
    | Type of TypeDeclaration

type Class = Declaration

type Scheme = string * Class
