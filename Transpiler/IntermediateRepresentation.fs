namespace Transpiler

type IrAxiomDeclaration =
    | IrQuantified of (Typing list * IrAxiomDeclaration)
    // TODO: Determine if the Accessor should be further specified
    | IrInfix of (Accessor * ValueExpression)

type IrTransitionRule =
    { Rule: ValueExpression
      NamedRules: Map<string, ValueExpression> }

type IrTransitionSystem =
    { Name: string
      Variable: Option<Map<string, ValueDeclaration>>
      InitConstraint: Option<IrAxiomDeclaration list>
      TransitionRule: Option<IrTransitionRule> }

[<Struct>]
type Intermediate =
    { Type: Option<Declaration>
      Value: Option<Map<string, ValueDeclaration>>
      Axiom: Option<IrAxiomDeclaration list>
      TransitionSystem: Option<IrTransitionSystem> }

