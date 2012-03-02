module Test4

type token =
  | NUMBER of float
  | OPERATOR of string
  | EOF of string

open FParsec

type innerTokenizer () = class
  member this.eof :Parser<token,unit> = 
    fun _ -> 
      new Reply<_>(EOF "")
  member this.number = pfloat |>> (fun x -> NUMBER x)
  member this.operator = 
    let f x : token = token.OPERATOR ((string)x)
    (pchar '+' <|> pchar '-' <|> pchar '*' <|> pchar '/') |>> f
  member this.run  : Parser<_,unit> = (many (this.operator <|> this.number))  .>> this.eof
end

open Falka.Comb
type innerLexer (lst : token list) = class
  interface ITokenLexer<token> with
    member this.is_empty () = List.isEmpty lst
    member this.peek () = List.head lst
    member this.tail () =
      if List.length lst = 1 && List.head lst = EOF ""
      then new innerLexer (lst) :> ITokenLexer<token>
      else new innerLexer (List.tail lst) :> ITokenLexer<token>
  (* next members will be invoked via Dynamic *)
  member this.number () : Result<float, token> =
    let o = this :> ITokenLexer<token>
    if o.is_empty () 
    then Failed "input is empty"
    else match o.peek () with
         | NUMBER x -> Success (x, o.tail ())
         | _ -> Failed "cant parse number"

  member this.operator () : Result<string, token> =
    let o = this :> ITokenLexer<token>
    if o.is_empty ()
    then Failed "input is empty"
    else match o.peek () with
         | OPERATOR p -> Success (p, o.tail ())
         | _ -> Failed "cant parse operator"
  member this.eof () : Result<string, token> =
    let o = this :> ITokenLexer<token>
    match o.peek () with
         | EOF p -> Success (p, o)
         | _ -> Failed "cant parse eof"
  override this.ToString () = lst.ToString ()
end

open Microsoft.FSharp.Compiler.Reflection
open Falka.Comb

type ast =
  | ANumber of float
  | AExpr of string * ast * ast

let wrap_rec p = 
  let _expr, exprImpl = createParserForwardedToRef()
  exprImpl.Value <- p _expr
  _expr

let wrap_meth s (f : Parser<_,_>) =
  f s

open Falka.Attributes
open Linq.QuotationEvaluation

(* An attempt to specify AST information without ReflectedDefinitionAttribute
 * Programmer should eithere define `falka` to use code with quotations
 * or not define it to use parser-combinators only.
 * N.B. Quotations brings some overhead to combinator parser
 *)
type parser_result =
  #if falka
  Quotations.Expr< Result<ast,token> >
  #else
  Result<ast,token>
  #endif

[<ParserClassAttribute("Expression", "Test4","")>]
type InnerParser () =
  [<LexerCombinator("NUMBER","float")>]
  member this.Number stream : Result<float, token> =
    (stream?number : unit -> Result<float,token>) ()
  [<LexerCombinator("EOF","string")>]
  member this.EOF stream : Result<string, token> =
    (stream?eof : unit -> Result<string,token>) ()
  [<LexerCombinator("OPERATOR","string")>]
  member this.Operator stream : Result<string, token> =
    (stream?operator : unit -> Result<string,token>) ()

  abstract member Expression2: ITokenLexer<token> -> parser_result
  [<ParserFunction>]
  default this.Expression2 stream =
    #if falka
    <@
    #endif
      (
        (pipe3 this.Number this.Operator this.Expression2_call (fun a op c -> AExpr (op, ANumber a,c)))
        <|> 
        (this.Number |>> (fun x -> ANumber x) )
      ) stream
    #if falka
    @>
    #endif

  member this.Expression2_call stream =
    #if falka
    (this.Expression2 stream).Eval()
    #else
    this.Expression2 stream
    #endif  
  
  abstract member Start2: ITokenLexer<token> -> parser_result
  default this.Start2 stream =
    #if falka
    <@
    #endif
     (this.Expression2_call .>> this.EOF) stream
    #if falka
    @>
    #endif
  member this.Start2_call stream =
    #if falka
    (this.Start2 stream).Eval()
    #else
    this.Start2 stream
    #endif  
 
