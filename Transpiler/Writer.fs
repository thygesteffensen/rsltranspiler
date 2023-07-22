module Transpiler.Writer

open System
open Transpiler.Ast
open System.IO

let getValueLiteralString =
    function
    | VUnit _ -> "()"
    | VBool b ->
        match b with
        | true -> "true"
        | false -> "false"
    | VInt i -> string i
    | VReal r -> string r
    | VChar c -> string c
    | VNat n -> string n
    | VText t -> t

let getTypeLiteralString =
    function
    | TUnit _ -> "Unit"
    | TBool -> "Bool"
    | TInt -> "Int"
    | TReal -> "Real"
    | TChar -> "Char"
    | TNat -> "Nat"
    | TText -> "Text"

let writeTypeExpression (stream: StreamWriter) _depth typeExpression =
    match typeExpression with
    | Literal lit -> stream.Write(getTypeLiteralString lit)
    | TName(n, _) -> stream.Write n
    | Product _ -> failwith "todo"
    | Set _ -> failwith "todo"
    | List _ -> failwith "todo"
    | Map _ -> failwith "todo"
    | TArray _ -> failwith "todo"
    | Sub _ -> failwith "todo"

let writeTyping (stream: StreamWriter) depth (typing: Typing) =
    match typing with
    | SingleTyping(identifier, typeExpression) ->
        match identifier with
        | IGeneric _ -> failwith "todo"
        | ISimple(id, _) ->
            stream.Write(id + " : ")
            writeTypeExpression stream depth typeExpression

let xx (stream: string -> Unit) (delimiter: string) = (fun () -> stream delimiter)

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

let rec writeValueExpression (stream: StreamWriter) depth valueExpression =
    match valueExpression with
    | ValueLiteral(literal, _) -> stream.Write(getValueLiteralString literal)
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

        writeValueExpression stream depth valueExpression

        stream.Write ")"
    | Infix(lhs, infixOp, rhs) ->
        stream.Write("(")
        writeValueExpression stream depth lhs

        match infixOp with
        | Equal -> stream.Write " = "
        | Plus -> stream.Write " + "
        | Minus -> stream.Write " - "
        | Guard -> stream.Write " ==> "
        | Deterministic -> stream.Write " [>] "
        | NonDeterministic ->
            stream.WriteLine ()
            stream.Write (String.replicate depth "\t")
            stream.WriteLine " [=] "
            stream.Write (String.replicate depth "\t")
        | LessThan -> stream.Write " < "
        | LessThanOrEqual -> stream.Write " <= "
        | GreaterThan -> stream.Write " > "
        | GreaterThanOrEqual -> stream.Write " >= "
        | Implies -> stream.Write " => "
        | LogicalAnd -> stream.Write " /\ "
        | LogicalOr -> stream.Write " \/ "

        writeValueExpression stream depth rhs
        stream.Write(")")
    | VeList valueExpressions ->
        listDelimiterAction (fun () -> stream.Write ", ") valueExpressions (writeValueExpression stream depth)
    | VArray _ -> failwith "todo"
    | LogicalNegation(valueExpression, _pos) ->
        stream.Write "~("
        writeValueExpression stream depth valueExpression
        stream.Write ")"

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
            stream.Write " := "
            writeValueExpression stream depth valueExpr
        | IGeneric _ -> failwith "todo"
    | ImplicitValue -> failwith "todo"
    | ExplicitFunction -> failwith "todo"
    | ImplicitFunction -> failwith "todo"
    | GenericValue(id, typingList, typeExpr) ->
        match id with
        | ISimple(id, _) ->
            // TODO: This one is a bit hacky due to 0 as depth as constructing a type
            stream.Write(id + " [ ")
            List.iter (fun e -> (writeValue stream 0 (Typing e))) typingList
            stream.Write " ] = "
            writeTypeExpression stream depth typeExpr
        | IGeneric _ -> failwith "todo"

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
    | Abstract -> stream.WriteLine(fst id)
    | Concrete _ -> failwith "todo"
    | Union l ->
        stream.Write((fst id) + " == ")
        listDelimiterAction (fun () -> stream.Write " | ") l stream.Write
        stream.WriteLine()
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

let writeDeclaration (stream: StreamWriter) depth decl =
    match decl with
    | Value valueDeclarations ->
        stream.WriteLine((String.replicate depth "\t") + "value")
        List.iter (fun e -> writeValue stream (depth + 1) e) valueDeclarations
    | TypeDeclaration typeDeclarations ->
        stream.WriteLine((String.replicate depth "\t") + "type")
        List.iter (fun e -> writeType stream (depth + 1) e) typeDeclarations
    | AxiomDeclaration valueExpressions ->
        stream.WriteLine((String.replicate depth "\t") + "axiom")
        List.iter (fun e -> writeValueExpression stream (depth + 1) e) valueExpressions
    | TransitionSystemDeclaration((id, _), transitionSystems) ->
        stream.WriteLine((String.replicate depth "\t") + "transition_system [" + id + "]")
        List.iter (fun e -> writeTransitionSystem stream (depth + 1) e) transitionSystems
        stream.WriteLine((String.replicate depth "\t") + "end")

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
    