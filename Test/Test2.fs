module Test2

// короче тут будет примерно так.
// ибо я не могу сделать норм комбинаторы и скрестить это fsyacc 
// то будут комбинаторы с токенайзером. Т.е. Надо где-то описать тип token
// и мой стрим будет стрим токенов. Где по динамикам будут вызываться парсер-функции как мемберы
// этого стрима токенов.
(* Начнем с парсинга арифметики. *) 
type token =
  | TNumber of float
  | TOperator of string
type ast =
  | ANumber of float
  | AExpr of string * ast * ast

open FParsec

type innerTokenizer () = class
  member this.number = pfloat |>> (fun x -> TNumber x)
  member this.operator = 
    let f x = TOperator ((string)x)
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
[<ParserClassAttribute("Expression", typeof<token>, "Number,Operator" )>]
type InnerParser () = class
  member this.Number stream : Result<token, token> = 
    (stream?number : unit -> Result<token,token>) ()
  member this.Operator stream : Result<token, token> = 
    (stream?operator : unit -> Result<token,token>) ()

  [<ParserFunction>]
  [<ReflectedDefinition>]  
  member this.Twonumbers stream =
    let body = this.Number >>. this.Number
    wrap_meth stream body
  
  abstract member Expression: Parser<token,ast>
  [<ParserFunction>]
  [<ReflectedDefinition>]  
  default this.Expression stream =
    let body = 
        (pipe3 this.Number this.Operator this.Expression (fun a b c -> 
          match (a,b) with
          | (TNumber a,TOperator x) -> AExpr (x, ANumber a,c)
          | _ -> failwith "some bug here")
        ) 
        <|> (this.Number |>> (function TNumber x -> ANumber x | _ -> failwith "some bug") )  
    wrap_meth stream body
  
end
