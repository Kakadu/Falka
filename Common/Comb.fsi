module Falka.Comb

type ITokenLexer<'token> = 
  abstract member is_empty : unit -> bool
  abstract member peek : unit -> 'token
  abstract member tail : unit -> ITokenLexer<'token>

type Result<'result,'token> =
  | Success of 'result * ITokenLexer<'token>
  | Failed of string

type Parser<'token, 'Result> = ITokenLexer<'token> -> Result<'Result, 'token>

val (>>.): Parser<'t,'r> -> Parser<'t,'q> -> Parser<'t,'q>
val (.>>): Parser<'t,'r> -> Parser<'t,'q> -> Parser<'t,'r>
val (.>>.): Parser<'t,'r> -> Parser<'t,'u> -> Parser<'t,'r*'u>
val (|>>): Parser<'t,'r> -> ('r -> 'b) -> Parser<'t,'b>
val (<|>): Parser<'t,'u> -> Parser<'t,'u> -> Parser<'t,'u>
val opt: Parser<'t,'r> -> Parser<'t, 'r option>

val pipe3
  : Parser<'t,'a> -> Parser<'t,'b> -> Parser<'t,'c> -> ('a -> 'b -> 'c -> 'd) -> Parser<'t,'d>

val many: Parser<'t,'res> -> Parser<'t,'res list>
