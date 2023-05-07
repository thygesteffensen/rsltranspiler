module Transpiler.Writer

open System
open System.IO

let getValueLiteralString = function
    | VUnit _ -> "()"
    | VBool b -> string(bool)
    | VInt i -> string(i)
    | VReal r -> string(r)
    | VChar c -> string(c)
    | VNat n -> string(n)
    | VText t -> t
    
let getTypeLiteralString = function
    | TUnit _ -> "Unit"
    | TBool -> "Bool"
    | TInt -> "Int"
    | TReal -> "Real"
    | TChar -> "Char"
    | TNat -> "Nat"
    | TText -> "Text"

let writeTypeExpression (stream: StreamWriter) depth typeExpression =
    match typeExpression with
    | Literal lit -> stream.Write (getTypeLiteralString lit )
    | TName n -> stream.Write n
    | Product l -> failwith "todo"
    | Set e -> failwith "todo"
    | List e -> failwith "todo"
    | Map e -> failwith "todo"


let rec writeValueExpression (stream: StreamWriter) depth valueExpression =
    match valueExpression with
    | ValueLiteral literal -> stream.Write (literal |> getValueLiteralString)

let rec writeValue (stream: StreamWriter) depth valueDeclaration =
    stream.Write(String.replicate depth "\t")

    match valueDeclaration with
    | ExplicitValue(id, typeExpr, valueExpr) ->
        stream.Write (id + " : ")
        writeTypeExpression stream depth typeExpr
        writeValueExpression stream depth valueExpr
        stream.Write ""
    | ImplicitValue -> failwith "todo"
    | ExplicitFunction -> failwith "todo"
    | ImplicitFunction -> failwith "todo"
    | GenericValue(id, typingList, typeExpr) as gv ->
        // TODO: This one is a bit hacky due to 0 as depth as constructing a type
        stream.Write (id + " [ ")
        List.iter (fun e -> (writeValue stream 0 (Typing e))) typingList
        stream.Write " ] = "
        writeTypeExpression stream depth typeExpr
        stream.Write ""
        
    | Typing(SingleTyping(s, typeExpression)) ->
        stream.Write(s + " : ")
        writeTypeExpression stream depth typeExpression
        stream.Write ""
    |> ignore


let writeType (stream: StreamWriter) depth (id: string, typeDefinition) =
    stream.Write(String.replicate depth "\t")

    match typeDefinition with
    | Abstract -> stream.WriteLine id
    | Concrete typeExpr -> failwith "todo"
    | Union idList -> stream.WriteLine(id + " = " + (idList |> String.concat " | "))
    |> ignore

let writeDeclaration (stream: StreamWriter) depth decl =
    match decl with
    | Value valueDeclarations ->
        stream.WriteLine((String.replicate depth "\t") + "value")
        List.iter (fun e -> writeValue stream (depth + 1) e) valueDeclarations
    | TypeDeclaration typeDeclarations ->
        stream.WriteLine((String.replicate depth "\t") + "type")
        List.iter (fun e -> writeType stream (depth + 1) e) typeDeclarations

let writeClass (stream: StreamWriter) depth cls =
    stream.WriteLine (String.replicate depth "\t" + "class")
    List.iter (fun e -> writeDeclaration stream (depth + 1) e) cls
    stream.WriteLine ("\n" + String.replicate depth "\t" + "end")

let write ((specification, cls): Scheme) =
    // Path.get
    // use streamWriter = new StreamWriter($"{specification}_unfolded.rsl", false)
    use streamWriter = new StreamWriter("/home/thyge/dev/rsltranspiler/TranspilerTest/Samples/ValueNat2.rsl", false)

    streamWriter.WriteLine $"scheme {specification}_unfolded ="

    writeClass streamWriter 1 cls

    streamWriter.Flush |> ignore
    streamWriter.Close |> ignore
