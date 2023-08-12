namespace FsLexYaccDemo.Ast

type Expr =
    | Addition of Expr * Expr
    | Subtraction of Expr * Expr
    | Multiplication of Expr * Expr
    | Integer of int
