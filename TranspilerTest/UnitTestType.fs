module TranspilerTest.Type

open NUnit.Framework
open Transpiler.Ast
open TranspilerTest.Common

let f1 = "TypeAbstract.rsl"
let f2 = "TypeConcrete.rsl"
let f3 = "TypeUnion.rsl"
let f4 = "TypeArray.rsl"
let f5 = "TypesAll.rsl"
let f6 = "TypeSubType.rsl"

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
         Scheme(("TypeArray", pos 1 8 7 f4), [ TypeDeclaration [ (("T", pos 4 13 57 f4), Concrete(TArray(TName ("Int", pos 4 23 67 f4), TName ("Text", pos 4 30 74 f4)))) ] ]) |]
      [| "Samples/" + f5
         Scheme(
             ("TypesAll", pos 1 8 7 f5),
             [ TypeDeclaration
                   [ (("T", pos 4 13 56 f5), Abstract)
                     (("T1", pos 5 13 72 f5), Concrete(TName ("Nat", pos 5 18 77 f5)))
                     (("T2", pos 6 13 95 f5), Union([ ("t1", pos 6 19 101 f5); ("t2", pos 6 24 106 f5) ])) ] ]
         ) |]
      [| "Samples/" + f6
         Scheme(
            ("TypeSubType", pos 1 8 7 f6),
            [ TypeDeclaration
                 [
                    ("ArrayIndex", pos 4 13 59 f6),
                      Concrete(
                          Sub(
                              [ SingleTyping(ISimple("i", pos 4 29 75 f6), TName("Int", pos 4 33 79 f6)) ],
                              Infix(
                                  Infix(
                                      VName(ASimple("i", pos 4 40 86 f6)),
                                      GreaterThanOrEqual,
                                      ValueLiteral(VInt 0, pos 4 45 91 f6)
                                  ),
                                  LogicalAnd,
                                  Infix(
                                      VName(ASimple("i", pos 4 50 96 f6)),
                                      LessThan,
                                      ValueLiteral(VInt 5, pos 4 54 100 f6)
                                  )
                              )
                          )
                      )
                 ] ])
      |] ]

[<TestCaseSource(nameof input)>]
let TestType source expected =
    let actual = testLexerAndParserFromFile source

    match actual with
    | Some t -> Assert.AreEqual(expected, t)
    | None -> Assert.Fail "Should succeed"
