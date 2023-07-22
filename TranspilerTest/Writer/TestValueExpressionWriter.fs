module TranspilerTest.Writer.TestValueExpressionWriter

open System.IO
open NUnit.Framework
open Transpiler
open Transpiler.Ast
open TranspilerTest.Common

let dummyPos = pos 1 2 3 "temp.rsl"

let input: obj[] list =
    [ [|
         Infix(
             Infix(
                 ValueLiteral(VInt 2, dummyPos),
                 GreaterThan,
                 Infix(VName(ASimple("max", dummyPos)), Minus, ValueLiteral(VInt 1, dummyPos))
             ),
             LogicalAnd,
             Infix(
                 ValueLiteral(VInt 4, dummyPos),
                 LessThan,
                 Infix(VName(ASimple("min", dummyPos)), Minus, ValueLiteral(VInt 1, dummyPos))
             )
         )
         "(2 > (max - 1)) /\\ (4 < (min - 1))" |] ]

[<TestCaseSource(nameof input)>]
let testValueExpressionWriter input (expected: string) =
    use memoryStream = new MemoryStream()
    use streamWriter = new StreamWriter(memoryStream)

    Writer.writeValueExpression streamWriter 0 input

    streamWriter.Flush()

    Assert.AreEqual(expected, System.Text.Encoding.UTF8.GetString(memoryStream.ToArray()))
