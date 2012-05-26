module Test2
(* Parser combiantos with tokenization phase *)

type token =
  | NUMBER of float
  | OPERATOR of string
  | EOF 


open FParsec

type innerTokenizer () = class
  member this.eof :Parser<token,unit> = 
    fun _ -> 
      new Reply<_>(EOF)
  member this.number = pfloat |>> fun x -> NUMBER x
  member this.operator = 
    let f x : token = token.OPERATOR <| string x
    (pchar '+' <|> pchar '-' <|> pchar '*' <|> pchar '/') |>> f
  member this.run  : Parser<_,unit> = 
    //printfn "JOPA"
    (this.operator <|> this.number) |> many   .>> this.eof
end

open Falka.Comb
type innerLexer (pos:int, lst : ResizeArray<token>) = class
  interface ITokenLexer<token> with
    member this.is_empty () = (ResizeArray.length lst <= pos)
    member this.peek () = lst.[pos]
    member this.tail () =
      if lst.[pos] = EOF  && (ResizeArray.length lst = pos+1)
      then new innerLexer (pos,lst) :> ITokenLexer<token>
      else new innerLexer (pos+1,lst) :> ITokenLexer<token>
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
  member this.eof () : Result<unit, token> =
    let o = this :> ITokenLexer<token>
    match o.peek () with
         | EOF -> Success ((), o)
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
[<ParserClassAttribute("Expression", "Test2","")>]
type InnerParser () = class
  [<LexerCombinator("NUMBER","float")>]
  member this.Number stream : Result<float, token> =
    (stream?number : unit -> Result<float,token>) ()
  [<LexerCombinator("EOF","unit")>]
  member this.EOF stream : Result<unit, token> =
    (stream?eof : unit -> Result<unit,token>) ()
  [<LexerCombinator("OPERATOR","string")>]
  member this.Operator stream : Result<string, token> =
    (stream?operator : unit -> Result<string,token>) ()
(*
  abstract member Twonumbers: ITokenLexer<token> -> Result<ast,token>
  [<ParserFunction>]
  [<ReflectedDefinition>]  
  default this.Twonumbers stream =
    let body = this.Number >>. this.Number |>> (fun s -> ANumber s)
    wrap_meth stream body
*)
  abstract member Expression: ITokenLexer<token> -> Result<ast,token>
  [<ParserFunction>]
  default this.Expression stream =
    let body = 
        (pipe3 this.Number this.Operator this.Expression (fun a op c -> AExpr (op, ANumber a,c)))
        <|> (this.Number |>> (fun x -> ANumber x) )
    wrap_meth stream body
  
  abstract member Start: ITokenLexer<token> -> Result<ast,token>
  default this.Start stream =
    let body = this.Expression .>> this.EOF
    wrap_meth stream body

end
