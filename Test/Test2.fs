module Test2

// короче тут будет примерно так.
// ибо я не могу сделать норм комбинаторы и скрестить это fsyacc 
// то будут комбинаторы с токенайзером. Т.е. Надо где-то описать тип token
// и мой стрим будет стрим токенов. Где по динамикам будут вызываться парсер-функции как мемберы
// этого стрима токенов.
(* Начнем с парсинга арифметики. *) 
module Token = 
  type token = 
    | TNumber of float
    | TOperator of string
  type ast = 
    | Number of float
    | Expr of string * ast * ast
open Token  
open FParsec

type innerTokenizer () = class
  member this.number = pfloat |>> (fun x -> Token.TNumber x)
  member this.operator = 
    let f x = TOperator ((string)x)
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
  member this.number () : Result<token, token> =
    let o = this :> ITokenLexer<token>
    if o.is_empty () 
    then Failed "input is empty"
    else match o.peek () with
         | TNumber x as ans -> 
             let temp = o.tail ()
             Success (ans, temp)
         | _ -> Failed "cant parse number"

  member this.operator () : Result<token, token> =
    let o = this :> ITokenLexer<token>
    if o.is_empty ()
    then Failed "input is empty"
    else match o.peek () with
         | TOperator p as ans -> 
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

open Falka.Attributes
[<ParserClassAttribute("expr", typeof<Token.token>, "number,operator" )>]
type innerParser () = class
  member this.number stream : Result<token, token> = 
    (stream?number : unit -> Result<token,token>) ()
  member this.operator stream : Result<token, token> = 
    (stream?operator : unit -> Result<token,token>) ()

  [<ParserFunction>]
  [<ReflectedDefinition>]  
  member this.twonumbers stream = 
    let body = this.number >>. this.number
    wrap_meth stream body
  
  [<ParserFunction>]
  [<ReflectedDefinition>]  
  member this.expr stream = 
    let body = 
        (pipe3 this.number this.operator this.expr (fun a b c -> 
          match (a,b) with
          | (TNumber a,TOperator x) -> Expr (x,Number a,c)
          | _ -> failwith "some bug here")
        ) 
        <|> (this.number |>> (function TNumber x -> Number x | _ -> failwith "some bug") )  
    wrap_meth stream body
  
end
