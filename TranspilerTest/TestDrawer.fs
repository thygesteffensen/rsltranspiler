
module TranspilerTest.AstDrawer

open NUnit.Framework
open FSharp.Text.Lexing
open AstDrawerLibrary.Library
open AstDrawerLibrary.AstDrawer
open Transpiler
open Transpiler.Reader

[<SetUp>]
let setup () = ()


let dp =
    { pos_bol = 1
      pos_cnum = 1
      pos_orig_lnum = 1
      pos_fname =  "" 
      pos_lnum = 1 }

[<Test>]
let TestType () =
    let input = "Samples/NamedTransitionRulesSimple.rsl"
    let actual =testLexerAndParserFromFile input
    
    let tree =
        match actual.Value with
        | (s, _pos), declarations -> Node($"Scheme {s}", List.foldBack declarationToNode declarations [])
        
    let postscriptActual = design tree |> toPSfast
    
    use textReader = new System.IO.StreamReader("Samples/NamedTransitionRulesSimple.ps")
    let postscriptExpected = textReader.ReadToEnd ()
    
    Assert.AreEqual(postscriptActual, postscriptExpected)
   