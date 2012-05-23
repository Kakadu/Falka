module Test2FParsec

open FParsec

type ast =
  | ANumber of int
  | AExpr of string * ast * ast

let (expr:Parser<ast,unit> ,exprImpl) = createParserForwardedToRef ()
let op = (pchar '+' <|> pchar '*')
let expr_helper = 
     ((followedBy (pint32 .>>. op))  >>. (pint32 .>>. op .>>. expr) )
       |>> (fun ((l,r),z) -> AExpr (string r,ANumber l,z))     
     <|>
     (pint32 |>> fun x -> ANumber x)
do exprImpl :=  expr_helper
  
[<ReflectedDefinitionAttribute>]
let asdf: Parser<_,unit> = (pint32 .>>. pchar 'c') >>. (pfloat .>>. pstring "asfd")





