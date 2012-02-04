module Test3
(* Parser combiantos with tokenization phase *)

type token =
  | GlobalVar of string // @@asfd
  | LocalVar  of string // @asdf
  | Ident     of string // asdf
  | LParen    of char   // '('
  | RParen    of char   // ')'
  | KW_SELECT of string // select
  | KW_FROM   of string // from
  | OP_PLUS   of char   // +
  | OP_MINUS  of char   // -
  | OP_COMMA  of char   // ,
  | EOF of string

module Tokenizer =
  open FParsec
  open System
  let first = function
    | '_' | '@' | '#' -> true
    | c when Char.IsLetter c  -> true
    | _ -> false
  let others = function
    | '$' -> true
    | c when Char.IsDigit c -> true
    | c -> first c
  let eof:   Parser<token,unit> = fun _ -> new Reply<_>(EOF "")
  let comma: Parser<token,unit> = pchar ',' |>> (fun c -> OP_COMMA c)
  let minus: Parser<token,unit> = pchar '-' |>> (fun c -> OP_MINUS c)
  let plus:  Parser<token,unit> = pchar '+' |>> (fun c -> OP_PLUS c)
  let from:  Parser<token,unit> = (pstring "from"   .>> spaces) |>> (fun s -> KW_FROM s)
  let select:Parser<token,unit> = (pstring "select" .>> spaces) |>> (fun s -> KW_SELECT s)
  let lparen:Parser<token,unit> = pchar '(' |>> (fun s -> LParen s)
  let rparen:Parser<token,unit> = pchar ')' |>> (fun s -> RParen s)
  let ident : Parser<token,unit> = 
    ((manySatisfy2 first others) .>> spaces) |>> (fun c -> Ident c)
  let globalVar: Parser<token,unit> = 
    pstring "@@" >>. (manySatisfy2 first others) .>> spaces |>> (fun s-> GlobalVar s)
  let localVar : Parser<token,unit> = 
    pstring  "@" >>. (manySatisfy2 first others) .>> spaces |>> (fun s -> LocalVar s)
  let start: Parser<token list,unit> = 
    //many (comma <|> minus <|> plus <|> from <|> select <|> lparen <|> rparen <|> ident <|> localVar <|> globalVar)
    many (select <|> ident)
(*
type innerTokenizer () = class
  member this.eof :Parser<token,unit> = 
    fun _ -> 
      new Reply<_>(EOF "")
  member this.number = pfloat |>> (fun x -> Number x)
  member this.operator = 
    let f x : token = token.Operator ((string)x)
    (pchar '+' <|> pchar '-' <|> pchar '*' <|> pchar '/') |>> f
  member this.run  : Parser<_,unit> = (many (this.operator <|> this.number))  .>> this.eof
end

open Falka.Comb
type innerLexer (lst : token list) = class
  interface ITokenLexer<token> with
    member this.is_empty () = List.isEmpty lst
    member this.peek () = List.head lst
    member this.tail () = new innerLexer (List.tail lst) :> ITokenLexer<token>
  (* next members will be invoked via Dynamic *)
  member this.number () : Result<float, token> =
    let o = this :> ITokenLexer<token>
    if o.is_empty () 
    then Failed "input is empty"
    else match o.peek () with
         | Number x -> Success (x, o.tail ())
         | _ -> Failed "cant parse number"

  member this.operator () : Result<string, token> =
    let o = this :> ITokenLexer<token>
    if o.is_empty ()
    then Failed "input is empty"
    else match o.peek () with
         | Operator p -> Success (p, o.tail ())
         | _ -> Failed "cant parse operator"
  member this.eof () : Result<string, token> =
    let o = this :> ITokenLexer<token>
    match o.peek () with
         | EOF p -> Success (p, o)
         | _ -> Failed "cant parse eof"
  override this.ToString () = lst.ToString ()
end

open Test
open Microsoft.FSharp.Compiler.Reflection
open Falka.Comb

let wrap_rec p = 
  let _expr, exprImpl = createParserForwardedToRef()
  exprImpl.Value <- p _expr
  _expr

let wrap_meth s (f : Parser<_,_>) =
  f s

open Falka.Attributes
[<ParserClassAttribute("Start", typeof<token>, "Number,Operator,EOF" )>]
type InnerParser () = class
  [<LexerCombinator("Number","float")>]
  member this.Number stream : Result<float, token> =
    (stream?number : unit -> Result<float,token>) ()
  [<LexerCombinator("EOF","string")>]
  member this.EOF stream : Result<string, token> =
    (stream?eof : unit -> Result<string,token>) ()
  [<LexerCombinator("Operator","string")>]
  member this.Operator stream : Result<string, token> =
    (stream?operator : unit -> Result<string,token>) ()

  abstract member Twonumbers: ITokenLexer<token> -> Result<ast,token>
  [<ParserFunction>]
  [<ReflectedDefinition>]  
  default this.Twonumbers stream =
    let body = this.Number >>. this.Number |>> (fun s -> ANumber s)
    wrap_meth stream body

  abstract member Expression: ITokenLexer<token> -> Result<ast,token>
  [<ParserFunction>]
  [<ReflectedDefinition>]
  default this.Expression stream =
    let body = 
        (pipe3 this.Number this.Operator this.Expression (fun a op c -> AExpr (op, ANumber a,c)))
        <|> (this.Number |>> (fun x -> ANumber x) )
    wrap_meth stream body
  
  abstract member Start: ITokenLexer<token> -> Result<ast,token>
  [<ParserFunction>]
  [<ReflectedDefinition>]
  default this.Start stream =
    let body = this.Expression .>> this.EOF
    wrap_meth stream body

end
*)