module TranspilerTest.Type

open NUnit.Framework
open Transpiler
open TranspilerTest.Common

let input: obj[] list =
    [ [| "Samples/TypeAbstract.rsl"
         Scheme("TypeAbstract", [ TypeDeclaration [ ("T", Abstract) ] ]) |]
      [| "Samples/TypeConcrete.rsl"
         Scheme("TypeConcrete", [ TypeDeclaration [ ("T", Concrete(TName "Nat")) ] ]) |]
      [| "Samples/TypeUnion.rsl"
         Scheme("TypeUnion", [ TypeDeclaration [ ("T", Union([ "t1"; "t2"; "t3" ])) ] ]) |]
      [| "Samples/TypesAll.rsl"
         Scheme(
             "TypesAll",
             [ TypeDeclaration [ ("T", Abstract); ("T1", Concrete(TName "Nat")); ("T2", Union([ "t1"; "t2" ])) ] ]
         ) |] ]

[<TestCaseSource(nameof input)>]
let TestType source expected =
    let actual = testLexerAndParserFromFile source

    match actual with
    | Some t -> Assert.AreEqual(expected, t)
    | None -> Assert.Fail "Should succeed"
