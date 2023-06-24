namespace Transpiler.Intermediate

open Transpiler.Ast

type IrAxiomDeclaration =
    | IrQuantified of (Typing list * IrAxiomDeclaration)
    // TODO: Determine if the Accessor should be further specified
    | IrInfix of (Accessor * ValueExpression)

type Choice =
    | NonDeterministic
    | Deterministic

type IrTransitionRules =
    | Node of IrTransitionRules * Choice * IrTransitionRules
    | Leaf of IrTransitionRule
and
    IrTransitionRule =
    | Guarded of ValueExpression * Effect list
    | Name of string
    | Quantified of Choice * Typing list * IrTransitionRule
    
and Effect = Accessor * ValueExpression

type OrderedMap<'key, 'value when 'key: comparison> =
    { KeyOrder: 'key list
      Map: Map<'key, 'value> }

type IrTransitionSystem =
    { Name: string
      Variable: Option<Map<int * string, ValueDeclaration>>
      InitConstraint: Option<IrAxiomDeclaration list>
      TransitionRule: Option<IrTransitionRules * Map<(int * string), IrTransitionRules>> }

[<Struct>]
type Intermediate =
    { Type: Option<Declaration>
      Value: Option<Map<(int * string), ValueDeclaration>>
      Axiom: Option<IrAxiomDeclaration list>
      TransitionSystem: Option<IrTransitionSystem> }

