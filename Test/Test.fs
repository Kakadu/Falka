module Test
(*
open Falka.Comb
open Falka.Common

let charHelper c (stream:FalkaLexer) =
  let e: string = stream.peek ()
  if e.Equals c
  then Parsed (c, stream.tail ())
  else Failed 
  
  
[<ParserClassAttribute>]
type innerParser () = class
    member this.digit (stream: FalkaLexer) =
      let e = stream.peek ()
      match System.Int32.TryParse e with
      | (true,_) -> Parsed (e, stream.tail ())
      | (false,_)  -> Failed

    member this.opPlus (stream: FalkaLexer)  = charHelper "+" stream
    member this.opMinus (stream: FalkaLexer) = charHelper "-" stream
    member this.opMul (stream: FalkaLexer)   = charHelper "*" stream
    member this.opDiv (stream: FalkaLexer)   = charHelper "/" stream

    member this.operator (stream: FalkaLexer) =
      (this.opPlus ||| this.opMinus ||| this.opMul ||| this.opDiv) stream

    member this.expr (stream:FalkaLexer) = 
      ( (this.digit >>> this.operator >>> this.expr) ||| this.digit ) stream
      
end
*)

open FParsec
open Falka.Attributes

let wrap_rec p =
  let _expr, exprImpl = createParserForwardedToRef()
  exprImpl.Value <- p _expr
  _expr

let wrap_meth s (f : Parser<_,unit>) =
  f s

[<ParserClass>]
type parser1 () = class

  [<ParserFunction>]
  [<ReflectedDefinition>]
  member this.sekv stream = 
    let body = many (this.lbra .>> this.rbra)
    wrap_meth stream body

(*      
  [<ParserFunction>]
  [<ReflectedDefinition>]
  member this.floatlist stream = 
    let body = pstring "[" >>. sepBy pfloat (pstring ",") .>> pstring "]"
    wrap_meth stream body

  [<ParserFunction>]
  [<ReflectedDefinition>]
  member this.number stream = 
    let body = pfloat >>. spaces
    wrap_meth stream body
*)
  [<ParserFunction>]
  [<ReflectedDefinition>]
  member this.lbra stream = 
    let body = pstring "("
    wrap_meth stream body

  [<ParserFunction>]
  [<ReflectedDefinition>]
  member this.lbra2 stream = 
    let body = this.lbra
    wrap_meth stream body

  [<ParserFunction>]
  [<ReflectedDefinition>]
  member this.rbra stream = 
    let body = pstring ")"
    wrap_meth stream body
    
  [<ParserFunction>]
  [<ReflectedDefinition>]
  member this.op stream = 
    let body = pchar '+' <|> pchar '-' <|> pchar '*'
    wrap_meth stream body
(*
  [<ParserFunction>]
  [<ReflectedDefinition>]
  member this.expr stream =    
    let body = wrap_rec (fun ans -> this.number <|> (this.number >>. this.op >>. ans) )
    wrap_meth stream body
*)
end

