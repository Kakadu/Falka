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
  member this.eof (): Result<string,Lexer.token> =
    let o = this :> ITokenLexer<Lexer.token>
    if o.is_empty ()
    then Failed "input is empty"
    else 
      let el = o.peek()
      match el with
         | EOF x -> Success (x, o.tail())
         | _    -> Failed (sprintf "cant parse EOF. %A is on then top" el)

  member this.kw_create (): Result<string,Lexer.token> =
    let o = this :> ITokenLexer<Lexer.token>
    if o.is_empty ()
    then Failed "input is empty"
    else 
      let el = o.peek()
      match el with
         | KW_CREATE x -> Success (x, o.tail())
         | _    -> Failed (sprintf "cant parse KW_CREATE. %A is on then top" el)

  member this.kw_function (): Result<string,Lexer.token> =
    let o = this :> ITokenLexer<Lexer.token>
    if o.is_empty ()
    then Failed "input is empty"
    else 
      let el = o.peek()
      match el with
         | KW_FUNCTION x -> Success (x, o.tail())
         | _    -> Failed (sprintf "cant parse KW_FUNCTION. %A is on then top" el)

  member this.kw_return (): Result<string,Lexer.token> =
    let o = this :> ITokenLexer<Lexer.token>
    if o.is_empty ()
    then Failed "input is empty"
    else 
      let el = o.peek()
      match el with
         | KW_RETURN x -> Success (x, o.tail())
         | _    -> Failed (sprintf "cant parse KW_RETURN. %A is on then top" el)

  member this.kw_select (): Result<string,Lexer.token> =
    let o = this :> ITokenLexer<Lexer.token>
    if o.is_empty ()
    then Failed "input is empty"
    else 
      let el = o.peek()
      match el with
         | KW_SELECT x -> Success (x, o.tail())
         | _    -> Failed (sprintf "cant parse KW_SELECT. %A is on then top" el)

  member this.kw_from (): Result<string,Lexer.token> =
    let o = this :> ITokenLexer<Lexer.token>
    if o.is_empty ()
    then Failed "input is empty"
    else 
      let el = o.peek()
      match el with
         | KW_FROM x -> Success (x, o.tail())
         | _    -> Failed (sprintf "cant parse KW_FROM. %A is on then top" el)

  member this.kw_begin (): Result<string,Lexer.token> =
    let o = this :> ITokenLexer<Lexer.token>
    if o.is_empty ()
    then Failed "input is empty"
    else 
      let el = o.peek()
      match el with
         | KW_BEGIN x -> Success (x, o.tail())
         | _    -> Failed (sprintf "cant parse KW_BEGIN. %A is on then top" el)

  member this.kw_end (): Result<string,Lexer.token> =
    let o = this :> ITokenLexer<Lexer.token>
    if o.is_empty ()
    then Failed "input is empty"
    else 
      let el = o.peek()
      match el with
         | KW_END x -> Success (x, o.tail())
         | _    -> Failed (sprintf "cant parse KW_END. %A is on then top" el)

  member this.string_const (): Result<string,Lexer.token> =
    let o = this :> ITokenLexer<Lexer.token>
    if o.is_empty ()
    then Failed "input is empty"
    else 
      let el = o.peek()
      match el with
         | STRING_CONST x -> Success (x, o.tail())
         | _    -> Failed (sprintf "cant parse STRING_CONST. %A is on then top" el)

  member this.dec_number (): Result<string,Lexer.token> =
    let o = this :> ITokenLexer<Lexer.token>
    if o.is_empty ()
    then Failed "input is empty"
    else 
      let el = o.peek()
      match el with
         | DEC_NUMBER x -> Success (x, o.tail())
         | _    -> Failed (sprintf "cant parse DEC_NUMBER. %A is on then top" el)

  member this.ident (): Result<string,Lexer.token> =
    let o = this :> ITokenLexer<Lexer.token>
    if o.is_empty ()
    then Failed "input is empty"
    else 
      let el = o.peek()
      match el with
         | IDENT x -> Success (x, o.tail())
         | _    -> Failed (sprintf "cant parse IDENT. %A is on then top" el)

  member this.globalvar (): Result<string,Lexer.token> =
    let o = this :> ITokenLexer<Lexer.token>
    if o.is_empty ()
    then Failed "input is empty"
    else 
      let el = o.peek()
      match el with
         | GLOBALVAR x -> Success (x, o.tail())
         | _    -> Failed (sprintf "cant parse GLOBALVAR. %A is on then top" el)

  member this.globaltempobj (): Result<string,Lexer.token> =
    let o = this :> ITokenLexer<Lexer.token>
    if o.is_empty ()
    then Failed "input is empty"
    else 
      let el = o.peek()
      match el with
         | GLOBALTEMPOBJ x -> Success (x, o.tail())
         | _    -> Failed (sprintf "cant parse GLOBALTEMPOBJ. %A is on then top" el)

  member this.localvar (): Result<string,Lexer.token> =
    let o = this :> ITokenLexer<Lexer.token>
    if o.is_empty ()
    then Failed "input is empty"
    else 
      let el = o.peek()
      match el with
         | LOCALVAR x -> Success (x, o.tail())
         | _    -> Failed (sprintf "cant parse LOCALVAR. %A is on then top" el)

  member this.tempobj (): Result<string,Lexer.token> =
    let o = this :> ITokenLexer<Lexer.token>
    if o.is_empty ()
    then Failed "input is empty"
    else 
      let el = o.peek()
      match el with
         | TEMPOBJ x -> Success (x, o.tail())
         | _    -> Failed (sprintf "cant parse TEMPOBJ. %A is on then top" el)

  member this.dot (): Result<string,Lexer.token> =
    let o = this :> ITokenLexer<Lexer.token>
    if o.is_empty ()
    then Failed "input is empty"
    else 
      let el = o.peek()
      match el with
         | DOT x -> Success (x, o.tail())
         | _    -> Failed (sprintf "cant parse DOT. %A is on then top" el)

  member this.comma (): Result<string,Lexer.token> =
    let o = this :> ITokenLexer<Lexer.token>
    if o.is_empty ()
    then Failed "input is empty"
    else 
      let el = o.peek()
      match el with
         | COMMA x -> Success (x, o.tail())
         | _    -> Failed (sprintf "cant parse COMMA. %A is on then top" el)

  member this.op_plus (): Result<string,Lexer.token> =
    let o = this :> ITokenLexer<Lexer.token>
    if o.is_empty ()
    then Failed "input is empty"
    else 
      let el = o.peek()
      match el with
         | OP_PLUS x -> Success (x, o.tail())
         | _    -> Failed (sprintf "cant parse OP_PLUS. %A is on then top" el)

  member this.op_eq (): Result<string,Lexer.token> =
    let o = this :> ITokenLexer<Lexer.token>
    if o.is_empty ()
    then Failed "input is empty"
    else 
      let el = o.peek()
      match el with
         | OP_EQ x -> Success (x, o.tail())
         | _    -> Failed (sprintf "cant parse OP_EQ. %A is on then top" el)

  member this.op_minus (): Result<string,Lexer.token> =
    let o = this :> ITokenLexer<Lexer.token>
    if o.is_empty ()
    then Failed "input is empty"
    else 
      let el = o.peek()
      match el with
         | OP_MINUS x -> Success (x, o.tail())
         | _    -> Failed (sprintf "cant parse OP_MINUS. %A is on then top" el)

  member this.op_div (): Result<string,Lexer.token> =
    let o = this :> ITokenLexer<Lexer.token>
    if o.is_empty ()
    then Failed "input is empty"
    else 
      let el = o.peek()
      match el with
         | OP_DIV x -> Success (x, o.tail())
         | _    -> Failed (sprintf "cant parse OP_DIV. %A is on then top" el)

  member this.lparen (): Result<string,Lexer.token> =
    let o = this :> ITokenLexer<Lexer.token>
    if o.is_empty ()
    then Failed "input is empty"
    else 
      let el = o.peek()
      match el with
         | LPAREN x -> Success (x, o.tail())
         | _    -> Failed (sprintf "cant parse LPAREN. %A is on then top" el)

  member this.rparen (): Result<string,Lexer.token> =
    let o = this :> ITokenLexer<Lexer.token>
    if o.is_empty ()
    then Failed "input is empty"
    else 
      let el = o.peek()
      match el with
         | RPAREN x -> Success (x, o.tail())
         | _    -> Failed (sprintf "cant parse RPAREN. %A is on then top" el)

  member this.lbracket (): Result<string,Lexer.token> =
    let o = this :> ITokenLexer<Lexer.token>
    if o.is_empty ()
    then Failed "input is empty"
    else 
      let el = o.peek()
      match el with
         | LBRACKET x -> Success (x, o.tail())
         | _    -> Failed (sprintf "cant parse LBRACKET. %A is on then top" el)

  member this.rbracket (): Result<string,Lexer.token> =
    let o = this :> ITokenLexer<Lexer.token>
    if o.is_empty ()
    then Failed "input is empty"
    else 
      let el = o.peek()
      match el with
         | RBRACKET x -> Success (x, o.tail())
         | _    -> Failed (sprintf "cant parse RBRACKET. %A is on then top" el)

  member this.semi (): Result<string,Lexer.token> =
    let o = this :> ITokenLexer<Lexer.token>
    if o.is_empty ()
    then Failed "input is empty"
    else 
      let el = o.peek()
      match el with
         | SEMI x -> Success (x, o.tail())
         | _    -> Failed (sprintf "cant parse SEMI. %A is on then top" el)



open Falka.Comb
open Falka.Attributes
open Microsoft.FSharp.Compiler.Reflection

let wrap_meth s (f : Parser<_,_>) = f s

module Ast = 
  type aBinOp = OpPlus | OpMinus
  type asqlexpr = 
    | ASqlValue of asqlvalue
    | ALocalVar of string
    | AGlobalVar of string
    | AIdent of string
    | ASelect of asqlexpr * asqlexpr
    | ABinOp of aBinOp * asqlexpr * asqlexpr
  and asqlvalue = 
    | ADecNumber of int
    | AStringConst of string
  and arootstmnt = 
    | ACreateFunction of string * asqlexpr list

[<ParserClassAttribute("SqlExpression", "Test3.Lexer", "Test3.Parser.Ast")>]
type InnerParser () = 
  [<LexerCombinator("EOF","string")>]
  member this.EOF stream : Result<string, token> =
    (stream?eof : unit -> Result<string,token>) ()
  [<LexerCombinator("KW_CREATE","string")>]
  member this.Kw_create stream : Result<string, token> =
    (stream?kw_create : unit -> Result<string,token>) ()
  [<LexerCombinator("KW_FUNCTION","string")>]
  member this.Kw_function stream : Result<string, token> =
    (stream?kw_function : unit -> Result<string,token>) ()
  [<LexerCombinator("KW_RETURN","string")>]
  member this.Kw_return stream : Result<string, token> =
    (stream?kw_return : unit -> Result<string,token>) ()
  [<LexerCombinator("KW_SELECT","string")>]
  member this.Kw_select stream : Result<string, token> =
    (stream?kw_select : unit -> Result<string,token>) ()
  [<LexerCombinator("KW_FROM","string")>]
  member this.Kw_from stream : Result<string, token> =
    (stream?kw_from : unit -> Result<string,token>) ()
  [<LexerCombinator("KW_BEGIN","string")>]
  member this.Kw_begin stream : Result<string, token> =
    (stream?kw_begin : unit -> Result<string,token>) ()
  [<LexerCombinator("KW_END","string")>]
  member this.Kw_end stream : Result<string, token> =
    (stream?kw_end : unit -> Result<string,token>) ()
  [<LexerCombinator("STRING_CONST","string")>]
  member this.String_const stream : Result<string, token> =
    (stream?string_const : unit -> Result<string,token>) ()
  [<LexerCombinator("DEC_NUMBER","string")>]
  member this.Dec_number stream : Result<string, token> =
    (stream?dec_number : unit -> Result<string,token>) ()
  [<LexerCombinator("IDENT","string")>]
  member this.Ident stream : Result<string, token> =
    (stream?ident : unit -> Result<string,token>) ()
  [<LexerCombinator("GLOBALVAR","string")>]
  member this.Globalvar stream : Result<string, token> =
    (stream?globalvar : unit -> Result<string,token>) ()
  [<LexerCombinator("GLOBALTEMPOBJ","string")>]
  member this.Globaltempobj stream : Result<string, token> =
    (stream?globaltempobj : unit -> Result<string,token>) ()
  [<LexerCombinator("LOCALVAR","string")>]
  member this.Localvar stream : Result<string, token> =
    (stream?localvar : unit -> Result<string,token>) ()
  [<LexerCombinator("TEMPOBJ","string")>]
  member this.Tempobj stream : Result<string, token> =
    (stream?tempobj : unit -> Result<string,token>) ()
  [<LexerCombinator("DOT","string")>]
  member this.Dot stream : Result<string, token> =
    (stream?dot : unit -> Result<string,token>) ()
  [<LexerCombinator("COMMA","string")>]
  member this.Comma stream : Result<string, token> =
    (stream?comma : unit -> Result<string,token>) ()
  [<LexerCombinator("OP_PLUS","string")>]
  member this.Op_plus stream : Result<string, token> =
    (stream?op_plus : unit -> Result<string,token>) ()
  [<LexerCombinator("OP_EQ","string")>]
  member this.Op_eq stream : Result<string, token> =
    (stream?op_eq : unit -> Result<string,token>) ()
  [<LexerCombinator("OP_MINUS","string")>]
  member this.Op_minus stream : Result<string, token> =
    (stream?op_minus : unit -> Result<string,token>) ()
  [<LexerCombinator("OP_DIV","string")>]
  member this.Op_div stream : Result<string, token> =
    (stream?op_div : unit -> Result<string,token>) ()
  [<LexerCombinator("LPAREN","string")>]
  member this.Lparen stream : Result<string, token> =
    (stream?lparen : unit -> Result<string,token>) ()
  [<LexerCombinator("RPAREN","string")>]
  member this.Rparen stream : Result<string, token> =
    (stream?rparen : unit -> Result<string,token>) ()
  [<LexerCombinator("LBRACKET","string")>]
  member this.Lbracket stream : Result<string, token> =
    (stream?lbracket : unit -> Result<string,token>) ()
  [<LexerCombinator("RBRACKET","string")>]
  member this.Rbracket stream : Result<string, token> =
    (stream?rbracket : unit -> Result<string,token>) ()
  [<LexerCombinator("SEMI","string")>]
  member this.Semi stream : Result<string, token> =
    (stream?semi : unit -> Result<string,token>) ()

  abstract member SqlExpression: ITokenLexer<token> -> Result<Ast.asqlexpr,token>
  [<ParserFunction>]
  [<ReflectedDefinition>]
  default this.SqlExpression stream =
    let body =
      (this.Ident |>> (fun s -> Ast.AIdent s))
//      <|> (this.Localvar  |>> (fun s -> Ast.ALocalVar s))
//      <|> (this.Globalvar |>> (fun s -> Ast.AGlobalVar s))
//      <|> (this.Lparen >>. this.SqlExpression .>> this.Rparen)
      <|>
      (this.Kw_select >>. this.SqlExpression .>> this.Kw_from .>>. this.Ident
        |>> (fun (e,where) -> Ast.ASelect (e, Ast.AIdent where) ) )

    wrap_meth stream body

  abstract member CreateFunction: ITokenLexer<token> -> Result<Ast.arootstmnt,token>
  //[<ParserFunction>]
  //[<ReflectedDefinition>]
  default this.CreateFunction stream =
    let body =
      this.Kw_create >>. this.Kw_function >>. this.Ident
       .>>.
       ((this.SqlExpression |>> (fun x -> [x]))
        <|>
        ((this.Kw_begin >>. (many this.SqlExpression)) .>> this.Kw_end)
       )
       |>> (fun x -> Ast.ACreateFunction x)
    wrap_meth stream body
  

