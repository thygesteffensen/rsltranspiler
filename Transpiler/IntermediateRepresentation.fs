namespace Transpiler

type IrAxiomDeclaration =
    | IrQuantified of (Typing list * IrAxiomDeclaration)
    // TODO: Determine if the Accessor should be further specified
    | IrInfix of (Accessor * ValueExpression)

type Choice =
    | NonDeterministic
    | Deterministic

type IrGc =
    { Guard: ValueExpression
      Effect: (Accessor * ValueExpression) list }
    
type IrRule =
    | Named of string
    | Guarded of IrGc
    | Quan of (Choice * Typing list * IrRule)
    
type IrTr =
    | Single of IrRule
    | Deterministic of IrTr list
    | Chain of IrTr * Choice * IrTr

type IrTransitionRule =
    { Rule: IrTr
      NamedRules: Map<string, IrTr> }

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

