module Transpiler.Writer

open System
open Transpiler.Ast
open Transpiler.Helpers
open System.IO

/// <summary>
/// Iterate through a list and call delimiter function between each element and call elementAction on each element
/// </summary>
/// <param name="delimiter"></param>
/// <param name="list"></param>
/// <param name="elementAction"></param>
let listDelimiterAction (delimiter: Unit -> Unit) list elementAction =
    match list with
    | [] -> ()
    | v :: vs ->
        elementAction v

        List.iter
            (fun e ->
                delimiter ()
                elementAction e)
            vs
/// <summary>
/// Iterate through a list and call delimiter function between each element and call elementAction on each element
/// </summary>
/// <param name="delimiter"></param>
/// <param name="seq"></param>
/// <param name="elementAction"></param>
let seqDelimiterAction (delimiter: Unit -> Unit) (seq: seq<'a>) elementAction =
    if Seq.isEmpty seq then
        printfn "Empty"
    else
        let head = Seq.head seq
        let tail = Seq.skip 1 seq
        elementAction head

        Seq.iter
            (fun e ->
                delimiter ()
                elementAction e)
            tail

let getTemporalModalOperatorsString (tmo: TemporalModalOperators) =
    match tmo with
    | Globally -> "G"
    | Finally -> "F"
    | Release -> "R"
    | Weak -> "W"
    | Mighty -> "M"

let getTypeLiteralString =
    function
    | TUnit _ -> "Unit"
    | TBool -> "Bool"
    | TInt -> "Int"
    | TReal -> "Real"
    | TChar -> "Char"
    | TNat -> "Nat"
    | TText -> "Text"

let xx (stream: string -> Unit) (delimiter: string) = (fun () -> stream delimiter)

let infixOpToString (infixOp: InfixOp) : string =
    match infixOp with
    | Equal -> " = "
    | Plus -> " + "
    | Minus -> " - "
    | Guard -> " ==> "
    | Deterministic -> " [>] "
    | NonDeterministic -> " [=] "
    | LessThan -> " < "
    | LessThanOrEqual -> " <= "
    | GreaterThan -> " > "
    | GreaterThanOrEqual -> " >= "
    | Implies -> " => "
    | LogicalAnd -> " /\ "
    | LogicalOr -> " \/ "
    | NotEqual -> " ~= "

let rec writeTypeExpression (stream: StreamWriter) depth typeExpression =
    match typeExpression with
    | Literal lit -> stream.Write(getTypeLiteralString lit)
    | TName(n, _) -> stream.Write n
    | Product typeExprs ->
        listDelimiterAction (fun () -> stream.Write " >< ") typeExprs (writeTypeExpression stream depth)
    | Set typeExpr ->
        writeTypeExpression stream depth typeExpr
        stream.Write "-set"
    | List typeExpr ->
        writeTypeExpression stream depth typeExpr
        stream.Write "-list"
    | Map(typeExpr1, typeExpr2) ->
        writeTypeExpression stream depth typeExpr1
        stream.Write " -m-> "
        writeTypeExpression stream depth typeExpr2
    | TArray(typeExpr1, typeExpr2) ->
        stream.Write "array "
        writeTypeExpression stream depth typeExpr1
        stream.Write " of "
        writeTypeExpression stream depth typeExpr2
    | Sub(typings, valueExpression) ->
        stream.Write "{| "
        listDelimiterAction (fun () -> stream.Write ", ") typings (writeTyping stream depth)
        stream.Write " :- "
        writeValueExpression stream depth valueExpression
        stream.Write " |}"

and writeTyping (stream: StreamWriter) depth (typing: Typing) =
    match typing with
    | SingleTyping(identifier, typeExpression) ->
        match identifier with
        | IGeneric((id, _), typings) ->
            stream.Write(id + " [ ")
            listDelimiterAction (fun () -> stream.Write ", ") typings (writeTyping stream depth)
            stream.Write(" ] : ")
            writeTypeExpression stream depth typeExpression
        | ISimple(id, _) ->
            stream.Write(id + " : ")
            writeTypeExpression stream depth typeExpression

and writeValueExpression1 (stream: StreamWriter) depth (inner: bool) valueExpression =
    match valueExpression with
    | ValueLiteral(literal, _) -> stream.Write(literalToString literal)
    | VName accessor -> writeAccessor stream depth accessor false
    | VPName accessor -> writeAccessor stream depth accessor true
    | Rule(id, _) -> stream.Write id
    | Quantified((quantifier, _), typings, valueExpression) ->
        stream.Write "("

        match quantifier with
        | All -> stream.Write "all "
        | Exists -> stream.Write "exists "
        | ExactlyOne -> stream.Write "exists! "
        | Quantifier.Deterministic -> stream.Write "[>] "
        | Quantifier.NonDeterministic -> stream.Write "[=] "

        listDelimiterAction (fun () -> stream.Write ", ") typings (writeTyping stream depth)
        stream.Write " :- "

        writeValueExpression1 stream depth true valueExpression

        stream.Write ")"
    | Infix(lhs, infixOp, rhs) ->
        if (not (infixOp.Equals(NonDeterministic))) && inner then
            stream.Write("(")

        writeValueExpression1 stream depth true lhs

        match infixOp with
        | Equal -> stream.Write " = "
        | Plus -> stream.Write " + "
        | Minus -> stream.Write " - "
        | Guard -> stream.Write " ==> "
        | Deterministic -> stream.Write " [>] "
        | NonDeterministic ->
            stream.WriteLine()
            stream.Write(String.replicate depth "\t")
            stream.WriteLine " [=] "
            stream.Write(String.replicate depth "\t")
        | LessThan -> stream.Write " < "
        | LessThanOrEqual -> stream.Write " <= "
        | GreaterThan -> stream.Write " > "
        | GreaterThanOrEqual -> stream.Write " >= "
        | Implies -> stream.Write " => "
        | LogicalAnd -> stream.Write " /\ "
        | LogicalOr -> stream.Write " \/ "
        | NotEqual -> stream.Write " ~= "

        writeValueExpression1 stream depth true rhs

        if (not (infixOp.Equals(NonDeterministic))) && inner then
            stream.Write(")")
    | VeList valueExpressions ->
        listDelimiterAction (fun () -> stream.Write ", ") valueExpressions (writeValueExpression1 stream depth true)
    | VArray valueExpressions ->
        stream.Write "{. "
        listDelimiterAction (fun () -> stream.Write ", ") valueExpressions (writeValueExpression1 stream depth true)
        stream.Write " .}"
    | LogicalNegation(valueExpression, _pos) ->
        stream.Write "~("
        writeValueExpression1 stream depth true valueExpression
        stream.Write ")"
    | Prefix((temporalModalOperators, _pos), valueExpression) ->
        (getTemporalModalOperatorsString temporalModalOperators) + "(" |> stream.Write
        writeValueExpression stream depth valueExpression
        stream.Write ")"
    | Flat(infixOp, valueExpressions) ->
        let ff () =
            stream.WriteLine (infixOpToString infixOp)
            stream.Write(String.replicate depth "\t")
        stream.Write "("
        listDelimiterAction (fun () -> ff ()) valueExpressions (writeValueExpression1 stream depth true)
        stream.Write ")"


and writeValueExpression (stream: StreamWriter) depth valueExpression =
    writeValueExpression1 stream depth false valueExpression

and writeAccessor (stream: StreamWriter) depth (accessor: Accessor) (prime: Boolean) =
    match accessor with
    | ASimple(id, _) ->
        stream.Write id

        match prime with
        | true -> stream.Write "'"
        | false -> ()
    | AGeneric((id, _), valueExpressions) ->
        stream.Write id

        match prime with
        | true -> stream.Write "'"
        | false -> ()

        stream.Write "["
        List.iter (writeValueExpression stream depth) valueExpressions
        stream.Write "]"


let rec writeValue (stream: StreamWriter) depth valueDeclaration =
    stream.Write(String.replicate depth "\t")

    match valueDeclaration with
    | ExplicitValue(id, typeExpr, valueExpr) ->
        match id with
        | ISimple(id, _) ->
            stream.Write(id + " : ")
            writeTypeExpression stream depth typeExpr
            stream.Write " = "
            writeValueExpression stream depth valueExpr
        | IGeneric((id, _pos), typings) ->
            stream.Write(id + " [ ")
            listDelimiterAction (fun () -> stream.Write ", ") typings (writeTyping stream depth)
            stream.Write(" ] : ")
            writeTypeExpression stream depth typeExpr
            stream.Write " = "
            writeValueExpression stream depth valueExpr

    | Typing(SingleTyping(s, typeExpression)) ->
        match s with
        | ISimple(id, _) ->
            stream.Write(id + " : ")
            writeTypeExpression stream depth typeExpression
        | IGeneric((id, _), typeExprs) ->
            stream.Write(id + " [ ")
            List.iter (fun e -> writeTyping stream depth e) typeExprs
            stream.Write(" ]")
            stream.Write(" : ")
            writeTypeExpression stream depth typeExpression
    |> ignore


let writeType (stream: StreamWriter) depth (id: Pos<string>, typeDefinition) =
    stream.Write(String.replicate depth "\t")

    match typeDefinition with
    | Abstract -> stream.Write(fst id)
    | Concrete typeExpression ->
        stream.Write(fst id)
        stream.Write(" = ")
        writeTypeExpression stream depth typeExpression
    | Union l ->
        stream.Write((fst id) + " == ")
        listDelimiterAction (fun () -> stream.Write " | ") l stream.Write
    |> ignore

let writeTransitionSystem (stream: StreamWriter) depth (tr: TransitionSystem) =

    match tr with
    | Variable valueDeclarations ->
        stream.WriteLine((String.replicate depth "\t") + "variable")

        listDelimiterAction (xx stream.WriteLine ",") valueDeclarations (writeValue stream (depth + 1))
        stream.WriteLine()

    | InitConstraint valueExpressions ->
        stream.WriteLine((String.replicate depth "\t") + "init_constraint")

        let rec treeWriter (stream: StreamWriter) depth (tree: ValueExpression) =
            match tree with
            | Infix(lhs, LogicalAnd, rhs) ->
                stream.Write(String.replicate depth "\t")
                writeValueExpression stream depth lhs
                stream.WriteLine(" /\\")
                treeWriter stream depth rhs
            | valueExpr ->
                stream.Write(String.replicate depth "\t")
                writeValueExpression stream depth valueExpr

        treeWriter stream (depth + 1) valueExpressions

        stream.WriteLine()

    | TransitionRule(valueExpression, tuples) ->
        stream.WriteLine((String.replicate depth "\t") + "transition_rules")

        stream.Write(String.replicate (depth + 1) "\t")
        writeValueExpression stream (depth + 1) valueExpression

        stream.WriteLine()

        match List.isEmpty tuples with
        | false ->
            stream.WriteLine((String.replicate (depth + 1) "\t") + "where")

            listDelimiterAction (xx stream.WriteLine ",") tuples (fun ((id, _), e) ->
                stream.WriteLine((String.replicate (depth + 2) "\t") + "[" + id + "] =")
                stream.Write(String.replicate (depth + 2) "\t")
                writeValueExpression stream (depth + 2) e)

            stream.WriteLine()
        | true -> ()

let writeLtlAssertion (stream: StreamWriter) depth (((name, _pos1), (ts, _pos2), valueExpr): LtlAssertion) =
    stream.WriteLine((String.replicate (depth + 1) "\t") + "[" + name + "] " + ts + " |-")
    stream.Write(String.replicate (depth + 2) "\t")
    writeValueExpression1 stream (depth + 1) true valueExpr

let writeDeclaration (stream: StreamWriter) depth decl =
    match decl with
    | Value valueDeclarations ->
        stream.WriteLine((String.replicate depth "\t") + "value")
        listDelimiterAction (xx stream.WriteLine ",") valueDeclarations (writeValue stream (depth + 1))
        stream.WriteLine()
    | TypeDeclaration typeDeclarations ->
        stream.WriteLine((String.replicate depth "\t") + "type")
        listDelimiterAction (xx stream.WriteLine ",") typeDeclarations (writeType stream (depth + 1))
        stream.WriteLine()
    | AxiomDeclaration valueExpressions ->
        stream.WriteLine((String.replicate depth "\t") + "axiom")
        listDelimiterAction (xx stream.WriteLine ",") valueExpressions (writeValueExpression stream (depth + 1))
        stream.WriteLine()
    | TransitionSystemDeclaration((id, _), transitionSystems) ->
        stream.WriteLine((String.replicate depth "\t") + "transition_system [" + id + "]")
        List.iter (fun e -> writeTransitionSystem stream (depth + 1) e) transitionSystems
        stream.WriteLine((String.replicate depth "\t") + "end")
    | LtlAssertionDeclaration ltlAssertions ->
        stream.WriteLine((String.replicate depth "\t") + "ltl_assertion")
        listDelimiterAction (xx stream.WriteLine ",") ltlAssertions (writeLtlAssertion stream (depth + 1))

let writeClass (stream: StreamWriter) depth cls =
    stream.WriteLine(String.replicate depth "\t" + "class")
    List.iter (fun e -> writeDeclaration stream (depth + 1) e) cls
    stream.WriteLine("\n" + String.replicate depth "\t" + "end")

let write (((specification, _), cls): Scheme) location =

    use streamWriter = new StreamWriter(location, false)

    streamWriter.WriteLine $"scheme {specification}_unfolded ="

    writeClass streamWriter 1 cls

    streamWriter.Flush |> ignore
    streamWriter.Close |> ignore

let writeAst ast location =
    use streamWriter = new StreamWriter(location, false)

    streamWriter.Write $"{ast}"

    streamWriter.Flush |> ignore
    streamWriter.Close |> ignore
