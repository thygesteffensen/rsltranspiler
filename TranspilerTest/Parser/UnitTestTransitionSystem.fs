module TranspilerTest.Parser.TransitionSystem

open NUnit.Framework
open Transpiler
open TranspilerTest.Common

let input: obj[] list =
    [ [| "Samples/TransitionSystem.rsl"
         Scheme(
             "TransitionSystem",
             [ TypeDeclaration([ ("Pos", Union([ "p1"; "p2" ])) ])
               TransitionSystemDeclaration(
                   ("TS",
                    [ Variable(
                          [ (Simple "v1", TName "Nat", None)
                            (Generic("v2", [ SingleTyping("t", TName "Pos") ]), TName "Nat", None)
                            (Simple("v3"), TName "Nat", Some(ValueLiteral(VInt 3))) ]
                      )
                      InitConstraint(
                          [ Infix(VName "v1", Equal, ValueLiteral(VInt 1))
                            Infix(VName "v3", Equal, ValueLiteral(VInt 3))
                            Quantified(
                                All,
                                [ SingleTyping("t", TName "Pos") ],
                                Infix(GenericName("v2", [ VName "t" ]), Equal, ValueLiteral(VInt 2))
                            ) ]
                      )
                      TransitionRule(
                          Infix(
                              Infix(
                                  Infix(ValueLiteral(VBool true), Equal, ValueLiteral(VBool true)),
                                  Guard,
                                  Infix(
                                      PGenericName("v2", [ VName "p1" ]),
                                      Equal,
                                      Infix(GenericName("v2", [ VName "p1" ]), Plus, ValueLiteral(VInt 1))
                                  )
                              ),
                              Deterministic,
                              Infix(
                                  Infix(
                                      Infix(ValueLiteral(VBool true), Equal, ValueLiteral(VBool false)),
                                      Guard,
                                      Infix(VPName "v1", Equal, Infix(VName "v3", Plus, ValueLiteral(VInt 1)))
                                  ),
                                  NonDeterministic,
                                  Quantified(
                                      Quantifier.NonDeterministic,
                                      [ SingleTyping("t", TName "Pos") ],
                                      Infix(
                                          Infix(ValueLiteral(VBool false), Equal, ValueLiteral(VBool false)),
                                          Guard,
                                          Infix(
                                              PGenericName("v2", [ VName "t" ]),
                                              Equal,
                                              Infix(GenericName("v2", [ VName "t" ]), Plus, ValueLiteral(VInt 1))
                                          )
                                      )
                                  )
                              )
                          ),
                          []
                      ) ])
               ) ]
         ) |] ]

[<TestCaseSource(nameof input)>]
let TestValueNat (source: string) expected =
    let actual = testLexerAndParserFromFile source

    match actual with
    | Some t -> Assert.AreEqual(expected, t)
    | None -> Assert.Fail "Should succeed"
