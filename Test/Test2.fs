module Test2

// короче тут будет примерно так.
// ибо я не могу сделать норм комбинаторы и скрестить это fsyacc 
// то будут комбинаторы с токенайзером. Т.е. Надо где-то описать тип token
// и мой стрим будет стрим токенов. Где по динамикам будут вызываться парсер-функции как мемберы
// этого стрима токенов.
(* Начнем с парсинга арифметики. *) 
type token =
  | Number of float
  | Operator of string
type ast =
  | ANumber of float
  | AExpr of string * ast * ast

open FParsec

type innerTokenizer () = class
  member this.number = pfloat |>> (fun x -> Number x)
  member this.operator = 
    let f x : token = token.Operator ((string)x)
    (pchar '+' <|> pchar '-' <|> pchar '*' <|> pchar '/') |>> f
  member this.run  : Parser<_,unit> = many (this.operator <|> this.number)
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
[<ParserClassAttribute("Expression", typeof<token>, "Number,Operator" )>]
type InnerParser () = class
  [<LexerCombinator("Number","float")>]
  member this.Number stream : Result<float, token> =
    (stream?number : unit -> Result<float,token>) ()
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
  
end
