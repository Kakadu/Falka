module Trash
(*
type token =
  | GlobalVar of string // @@asfd
  | LocalVar  of string // @asdf
  | Ident     of string // asdf
  | LParen    of char   // '('
  | RParen    of char   // ')'
  | KW_SELECT of string // select
  | KW_FROM   of string // from
  | OP_PLUS   of char   // +
  | OP_MINUS  of char   // -
  | OP_COMMA  of char   // ,
  | EOF of string

module Tokenizer =
  open FParsec
  open System
  let first = function
    | '_' | '@' | '#' -> true
    | c when Char.IsLetter c  -> true
    | _ -> false
  let others = function
    | '$' -> true
    | c when Char.IsDigit c -> true
    | c -> first c
  let eof:   Parser<token,unit> = fun _ -> new Reply<_>(EOF "")
  let comma: Parser<token,unit> = pchar ',' |>> (fun c -> OP_COMMA c)
  let minus: Parser<token,unit> = pchar '-' |>> (fun c -> OP_MINUS c)
  let plus:  Parser<token,unit> = pchar '+' |>> (fun c -> OP_PLUS c)
  let from:  Parser<token,unit> = (pstring "from"   .>> spaces) |>> (fun s -> KW_FROM s)
  let select:Parser<token,unit> = (pstring "select" .>> spaces) |>> (fun s -> KW_SELECT s)
  let lparen:Parser<token,unit> = pchar '(' |>> (fun s -> LParen s)
  let rparen:Parser<token,unit> = pchar ')' |>> (fun s -> RParen s)
  let ident : Parser<token,unit> = 
    ((many1Satisfy2 first others) .>> spaces) |>> (fun c -> Ident c)
  let globalVar: Parser<token,unit> = 
    pstring "@@" >>. (many1Satisfy2 first others) .>> spaces |>> (fun s-> GlobalVar s)
  let localVar : Parser<token,unit> = 
    pstring  "@" >>. (many1Satisfy2 first others) .>> spaces |>> (fun s -> LocalVar s)
  let start: Parser<token list,unit> = 
    //many (comma <|> minus <|> plus <|> from <|> select <|> lparen <|> rparen <|> ident <|> localVar <|> globalVar)
    many (select <|> from <|> ident )
    *)


