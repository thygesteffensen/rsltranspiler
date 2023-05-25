namespace Transpiler

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
    | Guarded of string * ValueExpression * (Accessor * ValueExpression) list
    | Name of string
    | Quantified of Choice * Typing list * IrTransitionRule

type IrTransitionSystem =
    { Name: string
      Variable: Option<Map<string, ValueDeclaration>>
      InitConstraint: Option<IrAxiomDeclaration list>
      TransitionRule: Option<IrTransitionRules * Map<string, IrTransitionRules>> }

[<Struct>]
type Intermediate =
    { Type: Option<Declaration>
      Value: Option<Map<string, ValueDeclaration>>
      Axiom: Option<IrAxiomDeclaration list>
      TransitionSystem: Option<IrTransitionSystem> }

