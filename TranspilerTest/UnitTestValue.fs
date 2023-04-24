module TranspilerTest.Value

open NUnit.Framework
open Transpiler
open TranspilerTest.Common

let input: obj[] list =
    [ [| "Samples/ValueNat.rsl"
         Scheme("ValueNat", [ (Value [ ExplicitValue("T", Name "Nat", None) ]) ]) |]
      [| "Samples/ValueGeneric.rsl"
         Scheme(
             "ValueGeneric",
             [ TypeDeclaration([ ("TrainId", Union([ "t1"; "t2"; "t3" ])) ])
               Value([ GenericValue("position", [ SingleTyping("t", Name "TrainId") ], Name "Nat") ]) ]
         ) |] ]

[<TestCaseSource(nameof input)>]
let TestValueNat (source: string) expected =
    let actual = testLexerAndParserFromFile source

    match actual with
    | Some t -> Assert.AreEqual(expected, t)
    | None -> Assert.Fail "Should succeed"
