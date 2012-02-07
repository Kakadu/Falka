// Signature file for parser generated by fsyacc
module GeneratedParser.Yacc
type token = 
  | Ident of (string)
  | EOF of (string)
  | Kw_from of (string)
  | Kw_select of (string)
type tokenId = 
    | TOKEN_Ident
    | TOKEN_EOF
    | TOKEN_Kw_from
    | TOKEN_Kw_select
    | TOKEN_end_of_input
    | TOKEN_error
type nonTerminalId = 
    | NONTERM__startyard_start_1
    | NONTERM_SqlExpression
    | NONTERM_yard_start_1
    | NONTERM_yard_exp_brackets_1
    | NONTERM_yard_exp_brackets_2
    | NONTERM_yard_exp_brackets_3
/// This function maps integers indexes to symbolic token ids
val tagOfToken: token -> int

/// This function maps integers indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val yard_start_1 : (Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> ('a) 