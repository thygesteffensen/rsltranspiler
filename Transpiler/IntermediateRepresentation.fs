namespace Transpiler

type IrAxiomDeclaration =
    | IrQuantified of (Typing list * IrAxiomDeclaration)
    // TODO: Determine if the Accessor should be further specified
    | IrInfix of (Accessor * ValueExpression)

[<Struct>]
type Intermediate =
    { Type: Option<Declaration>
      Value: Option<Map<string, ValueDeclaration>>
      Axiom: Option<IrAxiomDeclaration list> }

