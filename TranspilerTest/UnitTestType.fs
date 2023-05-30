module TranspilerTest.Type

open NUnit.Framework
open Transpiler.Ast
open TranspilerTest.Common

let f1 = "TypeAbstract.rsl"
let f2 = "TypeConcrete.rsl"
let f3 = "TypeUnion.rsl"
let f4 = "TypesAll.rsl"

let input: obj[] list =
    [ [| "Samples/" + f1
         Scheme(
             ("TypeAbstract", pos 1 8 7 "TypeAbstract.rsl"),
             [ TypeDeclaration [ (("T", pos 4 13 60 "TypeAbstract.rsl"), Abstract) ] ]
         ) |]
      [| "Samples/" + f2
         Scheme(("TypeConcrete", pos 1 8 7 f2), [ TypeDeclaration [ (("T", pos 4 13 60 f2), Concrete(TName ("Nat", pos 4 17 64 f2))) ] ]) |]
      [| "Samples/" + f3
         Scheme(("TypeUnion", pos 1 8 7 f3), [ TypeDeclaration [ (("T", pos 4 13 57 f3), Union([ ("t1", pos 4 18 62 f3); ("t2", pos 4 23 67 f3); ("t3", pos 4 28 72 f3) ])) ] ]) |]
      [| "Samples/" + f4
         Scheme(
             ("TypesAll", pos 1 8 7 f4),
             [ TypeDeclaration
                   [ (("T", pos 4 13 56 f4), Abstract)
                     (("T1", pos 5 13 72 f4), Concrete(TName ("Nat", pos 5 18 77 f4)))
                     (("T2", pos 6 13 95 f4), Union([ ("t1", pos 6 19 101 f4); ("t2", pos 6 24 106 f4) ])) ] ]
         ) |] ]

[<TestCaseSource(nameof input)>]
let TestType source expected =
    let actual = testLexerAndParserFromFile source

    match actual with
    | Some t -> Assert.AreEqual(expected, t)
    | None -> Assert.Fail "Should succeed"
