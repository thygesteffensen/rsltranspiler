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
             [ (Value [ Typing(SingleTyping(ISimple("T", pos 4 13 54 f1), TName("Nat", pos 4 17 58 f1))) ]) ]
         ) |]
      [| "Samples/" + f2
         Scheme(
             ("ValueGeneric", pos 1 8 7 f2),
             [ TypeDeclaration(
                   [ (("TrainId", pos 4 13 57 f2),
                      Union([ ("t1", pos 4 24 68 f2); ("t2", pos 4 29 73 f2); ("t3", pos 4 34 78 f2) ])) ]
               )
               Value(
                   [ Typing(
                         SingleTyping(
                             IGeneric(
                                 ("position", pos 6 13 107 f2),
                                 [ SingleTyping(ISimple("t", pos 6 24 118 f2), TName("TrainId", pos 6 28 122 f2)) ]
                             ),
                             TName("Nat", pos 6 40 134 f2)
                         )
                     ) ]
               ) ]
         ) |]
      [| "Samples/" + f3
         Scheme(
             ("ValuesAll", pos 1 8 7 f3),
             [ TypeDeclaration(
                   [ (("TrainId", pos 4 13 54 f3),
                      Union([ ("t1", pos 4 24 65 f3); ("t2", pos 4 29 70 f3); ("t3", pos 4 34 75 f3) ])) ]
               )
               Value(
                   [ Typing(
                         SingleTyping(
                             IGeneric(
                                 ("position", pos 6 13 104 f3),
                                 [ SingleTyping(ISimple("t", pos 6 24 115 f3), TName("TrainId", pos 6 28 119 f3)) ]
                             ),
                             TName("Nat", pos 6 40 131 f3)
                         )
                     )
                     Typing(SingleTyping(ISimple("segment", pos 7 13 148 f3), TName("Nat", pos 7 23 158 f3))) ]
               ) ]
         ) |]
      [| "Samples/" + f4
         Scheme(
             ("ValueArray", pos 1 8 7 f4),
             [ TypeDeclaration(
                   [ (("ArrayIndex", pos 4 13 55 f4),
                      Concrete(
                          Sub(
                              [ SingleTyping(ISimple("i", pos 4 29 71 f4), TName("Int", pos 4 33 75 f4)) ],
                              Infix(
                                  Infix(
                                      VName(ASimple("i", pos 4 40 82 f4)),
                                      GreaterThanOrEqual,
                                      ValueLiteral(VInt 0, pos 4 45 87 f4)
                                  ),
                                  LogicalAnd,
                                  Infix(
                                      VName(ASimple("i", pos 4 50 92 f4)),
                                      LessThan,
                                      ValueLiteral(VInt 5, pos 4 54 96 f4)
                                  )
                              )
                          )
                      ))
                     (("Array", pos 5 13 114 f4),
                      Concrete(TArray(TName("ArrayIndex", pos 5 27 128 f4), TName("Int", pos 5 41 142 f4)))) ]
               )
               Value(
                   [ ExplicitValue(
                         ISimple("position", pos 7 13 172 f4),
                         TName("Array", pos 7 24 183 f4),
                         ValueExpression.VArray(
                             [ ValueLiteral(VInt(1), pos 7 35 194 f4)
                               ValueLiteral(VInt(2), pos 7 38 197 f4)
                               ValueLiteral(VInt(3), pos 7 41 200 f4)
                               Infix(
                                   ValueLiteral(VInt(3), pos 7 44 203 f4),
                                   Plus,
                                   ValueLiteral(VInt(1), pos 7 48 207 f4)
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
