module Test2

// короче тут будет примерно так.
// ибо я не могу сделать норм комбинаторы и скрестить это fsyacc 
// то будут комбинаторы с токенайзером. Т.е. Надо где-то описать тип token
// и мой стрим будет стрим токенов. Где по динамикам будут вызываться парсер-функции как мемберы
// этого стрима токенов.
(* Начнем с парсинга арифметики. *) 
module Token = 
  type token = 
    | Number of float
    | Operator of string
open Token  
open FParsec

type innerTokenizer () = class
  member this.number = pfloat |>> (fun x -> Token.Number x)
  member this.operator = 
    let f x = Token.Operator ((string)x)
    (pchar '+' <|> pchar '-' <|> pchar '*' <|> pchar '/') |>> f
  member this.run  : Parser<_,unit> = many (this.operator <|> this.number)
end

open Falka.Comb
type innerLexer (lst : Token.token list) = class
  interface ITokenLexer<Token.token> with
    member this.is_empty () = List.isEmpty lst
    member this.peek () = List.head lst
    member this.tail () = new innerLexer (List.tail lst) :> ITokenLexer<Token.token>
  (* next members will be invoked via Dynamic *)
(*  static member operator (l: ITokenLexer<Token.token>) = 
      match l.peek () with
      | Operator x as ans -> Success (ans, l.tail ())
      | _ -> Failed "cant parse operator"
  static member number (l: ITokenLexer<Token.token>) = 
      match l.peek () with
      | Number x as ans -> Success (ans, l.tail ())
      | _ -> Failed "cant parse number"
      *)
  member this.number () : Result<token, token> =
    let o = this :> ITokenLexer<token>
    if o.is_empty () 
    then Failed "input is empty"
    else match o.peek () with
         | Number x as ans -> 
             let temp = o.tail ()
             Success (ans, temp)
         | _ -> Failed "cant parse number"

  member this.operator () : Result<token, token> =
    let o = this :> ITokenLexer<token>
    if o.is_empty ()
    then Failed "input is empty"
    else match o.peek () with
         | Operator p as ans -> 
             let temp = o.tail ()
             Success (ans, temp)
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

type innerParser () = class
  member this.number stream : Result<token, token> = 
    (stream?number : unit -> Result<token,token>) ()
  member this.operator stream : Result<token, token> = 
    (stream?operator : unit -> Result<token,token>) ()
  
  member this.expr stream = 
    let body = wrap_rec (fun ans ->   (this.number >>. this.operator >>. ans) <|> this.number)
    wrap_meth stream body
  
end
