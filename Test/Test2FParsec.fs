module Test2FParsec

open FParsec

type ast =
  | ANumber of int
  | AExpr of string * ast * ast

let (expr:Parser<ast,unit> ,exprImpl) = createParserForwardedToRef ()
do exprImpl := 
     pint32 |>> (fun x -> ANumber x)
     <|>
     pipe3 pint32 (pchar '+' <|> pchar '*') expr (fun n op e -> AExpr ((string)op,ANumber n, e)  )
  