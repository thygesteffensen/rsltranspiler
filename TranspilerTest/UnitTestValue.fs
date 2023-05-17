module TranspilerTest.Value

open NUnit.Framework
open Transpiler
open TranspilerTest.Common

let f1 = "ValueNat.rsl"
let f2 = "ValueGeneric.rsl"
let f3 = "ValuesAll.rsl"


let input: obj[] list =
    [ [| "Samples/" + f1 
         Scheme(("ValueNat", pos 1 8 7 f1), [ (Value [ Typing(SingleTyping(("T", pos 4 13 57 f1), TName ("Nat", pos 4 17 61 f1))) ]) ]) |]
      [| "Samples/" + f2 
         Scheme(
             ("ValueGeneric", pos 1 8 7 f2),
             [ TypeDeclaration([ (("TrainId", pos 4 13 60 f2), Union([ ("t1", pos 4 24 71 f2); ("t2", pos 4 29 76 f2); ("t3", pos 4 34 81 f2) ])) ])
               Value([ GenericValue(("position", pos 6 13 112 f2), [ SingleTyping(("t", pos 6 24 123 f2), TName ("TrainId", pos 6 28 127 f2)) ], TName ("Nat", pos 6 40 139 f2)) ]) ]
         ) |]
      [| "Samples/" + f3 
         Scheme(
             ("ValuesAll", pos 1 8 7 f3),
             [ TypeDeclaration([ (("TrainId", pos 4 13 57 f3), Union([ ("t1", pos 4 24 68 f3); ("t2", pos 4 29 73 f3); ("t3", pos 4 34 78 f3) ])) ])
               Value(
                   [ GenericValue(("position", pos 6 13 109 f3), [ SingleTyping(("t", pos 6 24 120 f3), TName ("TrainId",pos 6 28 124 f3)) ], TName ("Nat",pos 6 40 136 f3))
                     Typing(SingleTyping(("segment", pos 7 13 154 f3), TName ("Nat", pos 7 23 164 f3))) ]
               ) ]
         ) |] ]

[<TestCaseSource(nameof input)>]
let TestValueNat (source: string) expected =
    let actual = testLexerAndParserFromFile source

    match actual with
    | Some t -> Assert.AreEqual(expected, t)
    | None -> Assert.Fail "Should succeed"
