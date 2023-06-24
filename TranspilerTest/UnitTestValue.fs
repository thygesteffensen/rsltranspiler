module TranspilerTest.Value

open NUnit.Framework
open Transpiler.Ast
open TranspilerTest.Common

let f1 = "ValueNat.rsl"
let f2 = "ValueGeneric.rsl"
let f3 = "ValuesAll.rsl"
let f4 = "ValueArray.rsl"


let input: obj[] list =
    [ [| "Samples/" + f1
         Scheme(
             ("ValueNat", pos 1 8 7 f1),
             [ (Value [ Typing(SingleTyping(ISimple("T", pos 4 13 57 f1), TName("Nat", pos 4 17 61 f1))) ]) ]
         ) |]
      [| "Samples/" + f2
         Scheme(
             ("ValueGeneric", pos 1 8 7 f2),
             [ TypeDeclaration(
                   [ (("TrainId", pos 4 13 60 f2),
                      Union([ ("t1", pos 4 24 71 f2); ("t2", pos 4 29 76 f2); ("t3", pos 4 34 81 f2) ])) ]
               )
               Value(
                   [ Typing(
                         SingleTyping(
                             IGeneric(
                                 ("position", pos 6 13 112 f2),
                                 [ SingleTyping(ISimple("t", pos 6 24 123 f2), TName("TrainId", pos 6 28 127 f2)) ]
                             ),
                             TName("Nat", pos 6 40 139 f2)
                         )
                     ) ]
               ) ]
         ) |]
      [| "Samples/" + f3
         Scheme(
             ("ValuesAll", pos 1 8 7 f3),
             [ TypeDeclaration(
                   [ (("TrainId", pos 4 13 57 f3),
                      Union([ ("t1", pos 4 24 68 f3); ("t2", pos 4 29 73 f3); ("t3", pos 4 34 78 f3) ])) ]
               )
               Value(
                   [ Typing(
                         SingleTyping(
                             IGeneric(
                                 ("position", pos 6 13 109 f3),
                                 [ SingleTyping(ISimple("t", pos 6 24 120 f3), TName("TrainId", pos 6 28 124 f3)) ]
                             ),
                             TName("Nat", pos 6 40 136 f3)
                         )
                     )
                     Typing(SingleTyping(ISimple("segment", pos 7 13 154 f3), TName("Nat", pos 7 23 164 f3))) ]
               ) ]
         ) |]
      [| "Samples/" + f4
         Scheme(
             ("ValueArray", pos 1 8 7 f4),
             [ TypeDeclaration(
                   [ (("ArrayIndex", pos 4 13 58 f4),
                      Concrete(
                          Sub(
                              [ SingleTyping(ISimple("i", pos 4 29 74 f4), TName("Int", pos 4 33 78 f4)) ],
                              Infix(
                                  Infix(
                                      VName(ASimple("i", pos 4 40 85 f4)),
                                      GreaterThanOrEqual,
                                      ValueLiteral(VInt 0, pos 4 45 90 f4)
                                  ),
                                  LogicalAnd,
                                  Infix(
                                      VName(ASimple("i", pos 4 50 95 f4)),
                                      LessThan,
                                      ValueLiteral(VInt 5, pos 4 54 99 f4)
                                  )
                              )
                          )
                      ))
                     (("Array", pos 5 13 118 f4),
                      Concrete(TArray(TName("ArrayIndex", pos 5 27 132 f4), TName("Int", pos 5 41 146 f4)))) ]
               )
               Value(
                   [ ExplicitValue(
                         ISimple("position", pos 7 13 178 f4),
                         TName("Array", pos 7 24 189 f4),
                         ValueExpression.VArray(
                             [ ValueLiteral(VInt(1), pos 7 35 200 f4)
                               ValueLiteral(VInt(2), pos 7 38 203 f4)
                               ValueLiteral(VInt(3), pos 7 41 206 f4)
                               Infix(
                                   ValueLiteral(VInt(3), pos 7 44 209 f4),
                                   Plus,
                                   ValueLiteral(VInt(1), pos 7 48 213 f4)
                               ) ]
                         )
                     ) ]
               ) ]
         ) |] ]

[<TestCaseSource(nameof input)>]
let TestValueNat (source: string) expected =
    let actual = testLexerAndParserFromFile source

    match actual with
    | Some t -> Assert.AreEqual(expected, t)
    | None -> Assert.Fail "Should succeed"
