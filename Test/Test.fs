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

[<ParserClass>]
type parser1 () = class
  [<ParserFunction>]
  member this.floatlist : Parser<_,unit> = 
    pstring "[" >>. sepBy pfloat (pstring ",") .>> pstring "]"
  [<ParserFunction>]
  member this.number : Parser<_,unit> = 
    pfloat >>. spaces
  [<ParserFunction>]
  member this.lbra : Parser<_,unit> = pstring "("
  [<ParserFunction>]
  [<ReflectedDefinition>]
  member this.rbra : Parser<_,unit> = pstring ")"
  member this.op : Parser<_,unit> = 
    (pchar '+') <|> pchar '-' <|> pchar '*'
  member this.expr : Parser<_,unit> =
    wrap_rec (fun expr -> (this.number <|> (this.number >>. this.op >>. expr) ) )

end

(*
let ws = spaces // skips any whitespace

let str_ws s = pstring s >>. ws

// we calculate with double precision floats
let number = pfloat .>> ws

// we set up an operator precedence parser for parsing the arithmetic expressions
let opp = new OperatorPrecedenceParser<float,unit,unit>()
let expr = opp.ExpressionParser
opp.TermParser <- number <|> between (str_ws "(") (str_ws ")") expr

// operator definitions follow the schema
// operator type, string, trailing whitespace parser, precedence, associativity, function to apply

opp.AddOperator(InfixOperator("+", ws, 1, Associativity.Left, (+)))
opp.AddOperator(InfixOperator("-", ws, 1, Associativity.Left, (-)))
opp.AddOperator(InfixOperator("*", ws, 2, Associativity.Left, (*)))
opp.AddOperator(InfixOperator("/", ws, 2, Associativity.Left, (/)))
opp.AddOperator(InfixOperator("^", ws, 3, Associativity.Right, fun x y -> System.Math.Pow(x, y)))
opp.AddOperator(PrefixOperator("-", ws, 4, true, fun x -> -x))

// we also want to accept the operators "exp" and "log", but we don't want to accept
// expressions like "logexp" 2, so we require that non-symbolic operators are not
// followed by letters

let ws1 = nextCharSatisfiesNot isLetter >>. ws
opp.AddOperator(PrefixOperator("log", ws1, 4, true, System.Math.Log))
opp.AddOperator(PrefixOperator("exp", ws1, 4, true, System.Math.Exp))

let completeExpression = ws >>. expr .>> eof // we append the eof parser to make
                                            // sure all input is consumed

// running and testing the parser
/////////////////////////////////

let calculate s = run completeExpression s

let equals expectedValue r =
    match r with
    | Success (v, _, _) when v = expectedValue -> ()
    | Success (v, _, _)     -> failwith "Math is hard, let's go shopping!"
    | Failure (msg, err, _) -> printf "%s" msg; failwith msg

let test() =
    calculate "10.5 + 123.25 + 877"  |> equals 1010.75
    calculate "10/2 + 123.125 + 877" |> equals 1005.125
    calculate "(123 + log 1 + 877) * 9/3" |> equals 3000.
    calculate " ( ( exp 0 + (6 / ( 1 +2 ) )- 123456 )/ 2+123 + 877) * 3^2 / 3" |> equals (-182179.5)
    printfn "No errors"

// currently the program only executes some tests
do test()

*)