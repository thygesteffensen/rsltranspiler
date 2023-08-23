namespace Transpiler.Intermediate

open Transpiler.Ast

type IrAxiomDeclaration =
    | IrQuantified of (Typing list * IrAxiomDeclaration)
    | IrInfix of (Accessor * ValueExpression)

type Choice =
    | NonDeterministic
    | Deterministic

type IrTransitionRules =
    | Node of IrTransitionRules * Choice * IrTransitionRules
    | Leaf of IrTransitionRule

and IrTransitionRule =
    | Guarded of ValueExpression * Effect list
    | Name of string
    | Quantified of Choice * Typing list * IrTransitionRule

and Effect = Accessor * ValueExpression

/// <summary>
/// Map with type name as key and mapping to a tuple of the type definition and, if possible, the finite set of value
/// in the type expression.
/// </summary>
type TypeEnvMap = Map<string, TypeDefinition * ValueLiteral list>
type ValueEnvMap = Map<string, ValueLiteral>
type ValueDecMap = Map<int * string, ValueDeclaration>
type NamedRuleMap = Map<int * string, IrTransitionRules>

type IrTransitionSystem =
    { Name: string
      Variable: Option<ValueDecMap>
      InitConstraint: Option<IrAxiomDeclaration list>
      TransitionRule: Option<IrTransitionRules * NamedRuleMap> }

type Intermediate =
    { Type: Option<(Pos<Id> * TypeDefinition) list>
      Value: Option<ValueDecMap>
      Axiom: Option<IrAxiomDeclaration list>
      TransitionSystem: Option<IrTransitionSystem>
      LtlAssertion: Option<LtlAssertion list>  }
