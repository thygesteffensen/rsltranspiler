
module Transpiler.Helpers

let error_handler (tok: FSharp.Text.Parsing.ParseErrorContext<_>): unit =
    printfn $"Current token: {tok.CurrentToken} and {tok.ShiftTokens}"
    ()

