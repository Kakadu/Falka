// Signature file for parser generated by fsyacc
module GeneratedParser.Yacc
open Test2
type tokenId = 
    | TOKEN_Number
    | TOKEN_Operator
    | TOKEN_end_of_input
    | TOKEN_error
type nonTerminalId = 
    | NONTERM__startExpression
    | NONTERM_Twonumbers
    | NONTERM_Expression
    | NONTERM_yard_exp_brackets_1
/// This function maps integers indexes to symbolic token ids
val tagOfToken: token -> int

/// This function maps integers indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val Expression : (Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> ('a) 
