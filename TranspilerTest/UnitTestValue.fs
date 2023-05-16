module TranspilerTest.Value

open NUnit.Framework
open Transpiler
open TranspilerTest.Common


let temp =
    GenericValue("position", [ SingleTyping("t", TName "TrainId") ], TName "Nat")

let temp1 = Typing(SingleTyping("segment", TName "Nat"))

let temp2 =
    [ GenericValue("position", [ SingleTyping("t", TName "TrainId") ], TName "Nat")
      Typing(SingleTyping("segment", TName "Nat")) ]

let input: obj[] list =
    [ [| "Samples/ValueNat.rsl"
         Scheme("ValueNat", [ (Value [ Typing(SingleTyping("T", TName "Nat")) ]) ]) |]
      [| "Samples/ValueGeneric.rsl"
         Scheme(
             "ValueGeneric",
             [ TypeDeclaration([ ("TrainId", Union([ "t1"; "t2"; "t3" ])) ])
               Value([ GenericValue("position", [ SingleTyping("t", TName "TrainId") ], TName "Nat") ]) ]
         ) |]
      [| "Samples/ValuesAll.rsl"
         Scheme(
             "ValuesAll",
             [ TypeDeclaration([ ("TrainId", Union([ "t1"; "t2"; "t3" ])) ])
               Value(
                   [ GenericValue("position", [ SingleTyping("t", TName "TrainId") ], TName "Nat")
                     Typing(SingleTyping("segment", TName "Nat")) ]
               ) ]
         ) |] ]

[<TestCaseSource(nameof input)>]
let TestValueNat (source: string) expected =
    let actual = testLexerAndParserFromFile source

    match actual with
    | Some t -> Assert.AreEqual(expected, t)
    | None -> Assert.Fail "Should succeed"

let t =
    Infix(
        Infix(
            Infix(ValueLiteral(VBool true), Equal, ValueLiteral(VBool true)),
            Guard,
            Infix(
                VName(Generic("v2", [ VName (Simple "p1") ])),
                Equal,
                Infix(VName(Generic("v2", [ VName (Simple "p1") ])), Plus, ValueLiteral(VInt 1))
            )
        ),
        Deterministic,
        Infix(
            Infix(
                Infix(ValueLiteral(VBool true), Equal, ValueLiteral(VBool false)),
                Guard,
                Infix(VPName(Simple "v1"), Equal, Infix(VName(Simple "v3"), Plus, ValueLiteral(VInt 1)))
            ),
            NonDeterministic,
            Quantified(
                Quantifier.NonDeterministic,
                [ SingleTyping("t", TName "Pos") ],
                Infix(
                    Infix(ValueLiteral(VBool false), Equal, ValueLiteral(VBool false)),
                    Guard,
                    Infix(
                        VPName(Generic("v2", [ VName(Simple "t") ])),
                        Equal,
                        Infix(VName(Generic("v2", [ VName(Simple "t") ])), Plus, ValueLiteral(VInt 1))
                    )
                )
            )
        )
    )

let tt =
    Infix(
        Infix(
            Infix(ValueLiteral(VBool true), Equal, ValueLiteral(VBool true)),
            Guard,
            Infix(
                VPName(Generic("v2", [ VName(Simple "p1") ])),
                Equal,
                Infix(VName(Generic("v2", [ VName(Simple "p1") ])), Plus, ValueLiteral(VInt 1))
            )
        ),
        Deterministic,
        Infix(
            Infix(
                Infix(ValueLiteral(VBool true), Equal, ValueLiteral(VBool false)),
                Guard,
                Infix(VPName(Simple "v1"), Equal, Infix(VName(Simple "v3"), Plus, ValueLiteral(VInt 1)))
            ),
            NonDeterministic,
            Quantified(
                Quantifier.NonDeterministic,
                [ SingleTyping("t", TName("Pos")) ],
                Infix(
                    Infix(ValueLiteral(VBool false), Equal, ValueLiteral(VBool false)),
                    Guard,
                    Infix(
                        VPName(Generic("v2", [ VName(Simple "t") ])),
                        Equal,
                        Infix(VName(Generic("v2", [ VName(Simple "t") ])), Plus, ValueLiteral(VInt 1))
                    )
                )
            )
        )
    )
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    