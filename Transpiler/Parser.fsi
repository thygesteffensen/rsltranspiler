// Signature file for parser generated by fsyacc
module Parser
type token = 
  | EOF
  | COMMA
  | END
  | TYPE
  | CLASS
  | SCHEME
  | EQ
  | LT
  | LE
  | GT
  | GE
  | FALSE
  | TRUE
  | CHAR of (char)
  | TEXT of (string)
  | INT of (int)
  | ID of (string)
type tokenId = 
    | TOKEN_EOF
    | TOKEN_COMMA
    | TOKEN_END
    | TOKEN_TYPE
    | TOKEN_CLASS
    | TOKEN_SCHEME
    | TOKEN_EQ
    | TOKEN_LT
    | TOKEN_LE
    | TOKEN_GT
    | TOKEN_GE
    | TOKEN_FALSE
    | TOKEN_TRUE
    | TOKEN_CHAR
    | TOKEN_TEXT
    | TOKEN_INT
    | TOKEN_ID
    | TOKEN_end_of_input
    | TOKEN_error
type nonTerminalId = 
    | NONTERM__startstart
    | NONTERM_start
    | NONTERM_scheme
    | NONTERM_declarations
    | NONTERM_type_declarations
/// This function maps tokens to integer indexes
val tagOfToken: token -> int

/// This function maps integer indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val start : (FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> FSharp.Text.Lexing.LexBuffer<'cty> -> (Transpiler.Scheme option) 
