﻿namespace Transpiler

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

// AST Node
type TypeDefinition =
    | Abstract // Sort
    | Concrete of TypeExpression // Abbreviation t = Nat
    | Union of Id list

type Typing = SingleTyping of Id * TypeExpression

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
    | ValueLiteral of ValueLiteral
    | VName of Id
    | VPName of Id
    | GenericName of (Id * ValueExpression list)
    | PGenericName of (Id * ValueExpression list)
    | Equivalence of (ValueExpression * ValueExpression)
    | Quantified of (Quantifier * Typing list * ValueExpression)
    | Infix of (ValueExpression * InfixOp * ValueExpression)

// AST Node
type ValueDeclaration =
    | ExplicitValue of (Id * TypeExpression * ValueExpression)
    | ImplicitValue
    | ExplicitFunction
    | ImplicitFunction
    | GenericValue of (Id * Typing list * TypeExpression)
    | Typing of Typing

type Identifier =
    | Simple of string
    | Generic of (string * Typing list)

type TransitionSystem =
    | Variable of (Identifier * TypeExpression * Option<ValueExpression>) list
    | InitConstraint of ValueExpression list
    | TransitionRule of ValueExpression * (string * ValueExpression) list

type Declaration =
    | Value of ValueDeclaration list
    | TypeDeclaration of (Id * TypeDefinition) list
    | AxiomDeclaration of ValueExpression list
    | TransitionSystemDeclaration of (Id * TransitionSystem list)

type Class = Declaration list

type Scheme = Id * Class

[<Struct>]
type Intermediate =
    { Type: Option<Declaration>
      Value: Option<Map<string, ValueDeclaration>>
      Axiom: Option<Declaration> }
