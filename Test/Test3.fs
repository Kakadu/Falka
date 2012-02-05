module Test3.Parser
(* Parser combiantos with tokenization phase *)

open Test3.Lexer
open Falka.Comb
type innerLexer (lst : token list) = 
  interface ITokenLexer<token> with
    member this.is_empty () = List.isEmpty lst
    member this.peek () = List.head lst
    member this.tail () = new innerLexer (List.tail lst) :> ITokenLexer<token>
  override this.ToString () = lst.ToString ()
  (* next members will be invoked via Dynamic *)
  member this.eof : Result<string,Lexer.token> =
    let o = this :> ITokenLexer<Lexer.token>
    if o.is_empty ()
    then Failed "input is empty"
    else match o.peek ()  with
         | EOF x -> Success (x, o.tail())
         | _    -> Failed "cant parse EOF"

  member this.kw_select : Result<string,Lexer.token> =
    let o = this :> ITokenLexer<Lexer.token>
    if o.is_empty ()
    then Failed "input is empty"
    else match o.peek ()  with
         | KW_SELECT x -> Success (x, o.tail())
         | _    -> Failed "cant parse KW_SELECT"

  member this.kw_from : Result<string,Lexer.token> =
    let o = this :> ITokenLexer<Lexer.token>
    if o.is_empty ()
    then Failed "input is empty"
    else match o.peek ()  with
         | KW_FROM x -> Success (x, o.tail())
         | _    -> Failed "cant parse KW_FROM"

  member this.kw_begin : Result<string,Lexer.token> =
    let o = this :> ITokenLexer<Lexer.token>
    if o.is_empty ()
    then Failed "input is empty"
    else match o.peek ()  with
         | KW_BEGIN x -> Success (x, o.tail())
         | _    -> Failed "cant parse KW_BEGIN"

  member this.kw_end : Result<string,Lexer.token> =
    let o = this :> ITokenLexer<Lexer.token>
    if o.is_empty ()
    then Failed "input is empty"
    else match o.peek ()  with
         | KW_END x -> Success (x, o.tail())
         | _    -> Failed "cant parse KW_END"

  member this.string_const : Result<string,Lexer.token> =
    let o = this :> ITokenLexer<Lexer.token>
    if o.is_empty ()
    then Failed "input is empty"
    else match o.peek ()  with
         | STRING_CONST x -> Success (x, o.tail())
         | _    -> Failed "cant parse STRING_CONST"

  member this.dec_number : Result<string,Lexer.token> =
    let o = this :> ITokenLexer<Lexer.token>
    if o.is_empty ()
    then Failed "input is empty"
    else match o.peek ()  with
         | DEC_NUMBER x -> Success (x, o.tail())
         | _    -> Failed "cant parse DEC_NUMBER"

  member this.ident : Result<string,Lexer.token> =
    let o = this :> ITokenLexer<Lexer.token>
    if o.is_empty ()
    then Failed "input is empty"
    else match o.peek ()  with
         | IDENT x -> Success (x, o.tail())
         | _    -> Failed "cant parse IDENT"

  member this.globalvar : Result<string,Lexer.token> =
    let o = this :> ITokenLexer<Lexer.token>
    if o.is_empty ()
    then Failed "input is empty"
    else match o.peek ()  with
         | GLOBALVAR x -> Success (x, o.tail())
         | _    -> Failed "cant parse GLOBALVAR"

  member this.globaltempobj : Result<string,Lexer.token> =
    let o = this :> ITokenLexer<Lexer.token>
    if o.is_empty ()
    then Failed "input is empty"
    else match o.peek ()  with
         | GLOBALTEMPOBJ x -> Success (x, o.tail())
         | _    -> Failed "cant parse GLOBALTEMPOBJ"

  member this.localvar : Result<string,Lexer.token> =
    let o = this :> ITokenLexer<Lexer.token>
    if o.is_empty ()
    then Failed "input is empty"
    else match o.peek ()  with
         | LOCALVAR x -> Success (x, o.tail())
         | _    -> Failed "cant parse LOCALVAR"

  member this.tempobj : Result<string,Lexer.token> =
    let o = this :> ITokenLexer<Lexer.token>
    if o.is_empty ()
    then Failed "input is empty"
    else match o.peek ()  with
         | TEMPOBJ x -> Success (x, o.tail())
         | _    -> Failed "cant parse TEMPOBJ"

  member this.dot : Result<string,Lexer.token> =
    let o = this :> ITokenLexer<Lexer.token>
    if o.is_empty ()
    then Failed "input is empty"
    else match o.peek ()  with
         | DOT x -> Success (x, o.tail())
         | _    -> Failed "cant parse DOT"

  member this.comma : Result<string,Lexer.token> =
    let o = this :> ITokenLexer<Lexer.token>
    if o.is_empty ()
    then Failed "input is empty"
    else match o.peek ()  with
         | COMMA x -> Success (x, o.tail())
         | _    -> Failed "cant parse COMMA"

  member this.op_plus : Result<string,Lexer.token> =
    let o = this :> ITokenLexer<Lexer.token>
    if o.is_empty ()
    then Failed "input is empty"
    else match o.peek ()  with
         | OP_PLUS x -> Success (x, o.tail())
         | _    -> Failed "cant parse OP_PLUS"

  member this.op_eq : Result<string,Lexer.token> =
    let o = this :> ITokenLexer<Lexer.token>
    if o.is_empty ()
    then Failed "input is empty"
    else match o.peek ()  with
         | OP_EQ x -> Success (x, o.tail())
         | _    -> Failed "cant parse OP_EQ"

  member this.op_minus : Result<string,Lexer.token> =
    let o = this :> ITokenLexer<Lexer.token>
    if o.is_empty ()
    then Failed "input is empty"
    else match o.peek ()  with
         | OP_MINUS x -> Success (x, o.tail())
         | _    -> Failed "cant parse OP_MINUS"

  member this.op_div : Result<string,Lexer.token> =
    let o = this :> ITokenLexer<Lexer.token>
    if o.is_empty ()
    then Failed "input is empty"
    else match o.peek ()  with
         | OP_DIV x -> Success (x, o.tail())
         | _    -> Failed "cant parse OP_DIV"

  member this.lparen : Result<string,Lexer.token> =
    let o = this :> ITokenLexer<Lexer.token>
    if o.is_empty ()
    then Failed "input is empty"
    else match o.peek ()  with
         | LPAREN x -> Success (x, o.tail())
         | _    -> Failed "cant parse LPAREN"

  member this.rparen : Result<string,Lexer.token> =
    let o = this :> ITokenLexer<Lexer.token>
    if o.is_empty ()
    then Failed "input is empty"
    else match o.peek ()  with
         | RPAREN x -> Success (x, o.tail())
         | _    -> Failed "cant parse RPAREN"

  member this.lbracket : Result<string,Lexer.token> =
    let o = this :> ITokenLexer<Lexer.token>
    if o.is_empty ()
    then Failed "input is empty"
    else match o.peek ()  with
         | LBRACKET x -> Success (x, o.tail())
         | _    -> Failed "cant parse LBRACKET"

  member this.rbracket : Result<string,Lexer.token> =
    let o = this :> ITokenLexer<Lexer.token>
    if o.is_empty ()
    then Failed "input is empty"
    else match o.peek ()  with
         | RBRACKET x -> Success (x, o.tail())
         | _    -> Failed "cant parse RBRACKET"

  member this.semi : Result<string,Lexer.token> =
    let o = this :> ITokenLexer<Lexer.token>
    if o.is_empty ()
    then Failed "input is empty"
    else match o.peek ()  with
         | SEMI x -> Success (x, o.tail())
         | _    -> Failed "cant parse SEMI"

open Falka.Comb
open Falka.Attributes
open Microsoft.FSharp.Compiler.Reflection

let wrap_meth s (f : Parser<_,_>) = f s

module Ast = 
  type aBinOp = OpPlus | OpMinus
  type asqlexpr = 
    | SqlValue of asqlvalue
    | LocalVar of string
    | GlobalVar of string
    | Ident of string
    | Select of asqlexpr * asqlexpr
    | BinOp of aBinOp * asqlexpr * asqlexpr
  and asqlvalue = 
    | DecNumber of int
    | StringConst of string

[<ParserClassAttribute("Start", typeof<token>, "Number,Operator,EOF" )>]
type InnerParser () = 
  [<LexerCombinator("EOF","Lexer.token")>]
  member this.Eof stream : Result<string, token> =
    (stream?operator : unit -> Result<string,token>) ()
  [<LexerCombinator("KW_SELECT","Lexer.token")>]
  member this.Kw_select stream : Result<string, token> =
    (stream?operator : unit -> Result<string,token>) ()
  [<LexerCombinator("KW_FROM","Lexer.token")>]
  member this.Kw_from stream : Result<string, token> =
    (stream?operator : unit -> Result<string,token>) ()
  [<LexerCombinator("KW_BEGIN","Lexer.token")>]
  member this.Kw_begin stream : Result<string, token> =
    (stream?operator : unit -> Result<string,token>) ()
  [<LexerCombinator("KW_END","Lexer.token")>]
  member this.Kw_end stream : Result<string, token> =
    (stream?operator : unit -> Result<string,token>) ()
  [<LexerCombinator("STRING_CONST","Lexer.token")>]
  member this.String_const stream : Result<string, token> =
    (stream?operator : unit -> Result<string,token>) ()
  [<LexerCombinator("DEC_NUMBER","Lexer.token")>]
  member this.Dec_number stream : Result<string, token> =
    (stream?operator : unit -> Result<string,token>) ()
  [<LexerCombinator("IDENT","Lexer.token")>]
  member this.Ident stream : Result<string, token> =
    (stream?operator : unit -> Result<string,token>) ()
  [<LexerCombinator("GLOBALVAR","Lexer.token")>]
  member this.Globalvar stream : Result<string, token> =
    (stream?operator : unit -> Result<string,token>) ()
  [<LexerCombinator("GLOBALTEMPOBJ","Lexer.token")>]
  member this.Globaltempobj stream : Result<string, token> =
    (stream?operator : unit -> Result<string,token>) ()
  [<LexerCombinator("LOCALVAR","Lexer.token")>]
  member this.Localvar stream : Result<string, token> =
    (stream?operator : unit -> Result<string,token>) ()
  [<LexerCombinator("TEMPOBJ","Lexer.token")>]
  member this.Tempobj stream : Result<string, token> =
    (stream?operator : unit -> Result<string,token>) ()
  [<LexerCombinator("DOT","Lexer.token")>]
  member this.Dot stream : Result<string, token> =
    (stream?operator : unit -> Result<string,token>) ()
  [<LexerCombinator("COMMA","Lexer.token")>]
  member this.Comma stream : Result<string, token> =
    (stream?operator : unit -> Result<string,token>) ()
  [<LexerCombinator("OP_PLUS","Lexer.token")>]
  member this.Op_plus stream : Result<string, token> =
    (stream?operator : unit -> Result<string,token>) ()
  [<LexerCombinator("OP_EQ","Lexer.token")>]
  member this.Op_eq stream : Result<string, token> =
    (stream?operator : unit -> Result<string,token>) ()
  [<LexerCombinator("OP_MINUS","Lexer.token")>]
  member this.Op_minus stream : Result<string, token> =
    (stream?operator : unit -> Result<string,token>) ()
  [<LexerCombinator("OP_DIV","Lexer.token")>]
  member this.Op_div stream : Result<string, token> =
    (stream?operator : unit -> Result<string,token>) ()
  [<LexerCombinator("LPAREN","Lexer.token")>]
  member this.Lparen stream : Result<string, token> =
    (stream?operator : unit -> Result<string,token>) ()
  [<LexerCombinator("RPAREN","Lexer.token")>]
  member this.Rparen stream : Result<string, token> =
    (stream?operator : unit -> Result<string,token>) ()
  [<LexerCombinator("LBRACKET","Lexer.token")>]
  member this.Lbracket stream : Result<string, token> =
    (stream?operator : unit -> Result<string,token>) ()
  [<LexerCombinator("RBRACKET","Lexer.token")>]
  member this.Rbracket stream : Result<string, token> =
    (stream?operator : unit -> Result<string,token>) ()
  [<LexerCombinator("SEMI","Lexer.token")>]
  member this.Semi stream : Result<string, token> =
    (stream?operator : unit -> Result<string,token>) ()

(*
  abstract member Twonumbers: ITokenLexer<token> -> Result<ast,token>
  [<ParserFunction>]
  [<ReflectedDefinition>]  
  default this.Twonumbers stream =
    let body = this.Number >>. this.Number |>> (fun s -> ANumber s)
    wrap_meth stream body
*)

