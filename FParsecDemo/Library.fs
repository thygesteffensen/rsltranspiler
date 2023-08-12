module ClassLibrary1

#r "nuget:FParsec"
open FParsec

type Expr =
    | Addition of Expr * Expr
    | Subtraction of Expr * Expr
    | Multiplication of Expr * Expr
    | Integer of int

let constructExpr char expr1 expr2 =
    match char with
    | '+' -> Addition(expr1, expr2)
    | '-' -> Subtraction(expr1, expr2)
    | '*' -> Multiplication(expr1, expr2)
    | _ -> failwith $"Unknown character '{char}' encountered."

let parseOp = (pchar '+' <|> pchar '-' <|> pchar '*') .>> spaces
let parserInteger = pint32 |>> Integer .>> spaces

let parseExpr, parseExprRef = createParserForwardedToRef ()

let expr: Parser<Expr, unit> = parseExpr <|> parserInteger

do
    parseExprRef.Value <-  pipe3 parseOp expr expr constructExpr .>> spaces

let runParser input =
    match run expr input with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Error: %s" errorMsg

runParser "+ * 5 6 7"

(*
module ClassLibrary1

open FParsec

type Expr =
    | Add of Expr * Expr
    | Num of int

let number: Parser<Expr, unit> =
    pfloat
    |> map (fun n -> Num(int n))

let addExpr: Parser<Expr, unit> =
    pipe2
        (number .>> spaces1)
        (pchar '+' .>>. spaces1 >>. number)
        (fun n1 n2 -> Add(n1, n2))


let rec addExpr: Parser<Expr, unit> =
    pipe2
        (expr .>> spaces1)
        (pchar '+' .>>. spaces1 >>. expr)
        (fun e1 e2 -> Add(e1, e2))

and expr: Parser<Expr, unit> = 
    number <|> addExpr
    *)
