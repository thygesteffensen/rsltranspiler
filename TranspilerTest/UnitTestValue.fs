module TranspilerTest.Value

open NUnit.Framework
open Transpiler
open TranspilerTest.Common


let temp =
    GenericValue("position", [ SingleTyping("t", Name "TrainId") ], Name "Nat")

let temp1 = Typing(SingleTyping("segment", Name "Nat"))

let temp2 =
    [ GenericValue("position", [ SingleTyping("t", Name "TrainId") ], Name "Nat")
      Typing(SingleTyping("segment", Name "Nat")) ]

let input: obj[] list =
    [ [| "Samples/ValueNat.rsl"
         Scheme("ValueNat", [ (Value [ Typing(SingleTyping("T", Name "Nat")) ]) ]) |]
      [| "Samples/ValueGeneric.rsl"
         Scheme(
             "ValueGeneric",
             [ TypeDeclaration([ ("TrainId", Union([ "t1"; "t2"; "t3" ])) ])
               Value([ GenericValue("position", [ SingleTyping("t", Name "TrainId") ], Name "Nat") ]) ]
         ) |]
      [| "Samples/ValuesAll.rsl"
         Scheme(
             "ValuesAll",
             [ TypeDeclaration([ ("TrainId", Union([ "t1"; "t2"; "t3" ])) ])
               Value(
                   [ GenericValue("position", [ SingleTyping("t", Name "TrainId") ], Name "Nat")
                     Typing(SingleTyping("segment", Name "Nat")) ]
               ) ]
         ) |] ]

[<TestCaseSource(nameof input)>]
let TestValueNat (source: string) expected =
    let actual = testLexerAndParserFromFile source

    match actual with
    | Some t -> Assert.AreEqual(expected, t)
    | None -> Assert.Fail "Should succeed"
