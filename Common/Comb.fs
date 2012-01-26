module Falka.Comb

type ITokenLexer<'token> = 
  abstract member is_empty : unit -> bool
  abstract member peek : unit -> 'token
  abstract member tail : unit -> ITokenLexer<'token>

type Result<'a,'token> = 
  | Success of 'a * ITokenLexer<'token>
  | Failed of string

type Parser<'token, 'Result> = ITokenLexer<'token> -> Result<'Result, 'token>

let (>>.) p1 p2 = fun lst ->
  match p1 lst with
  | Success (_, tail) -> p2 tail
  | Failed _ as x -> x
  
let (.>>.) p1 p2 = fun lst ->
  match p1 lst with
  | Success (ans1, tail) -> 
      match p2 tail with
      | Success (ans2, tail) -> Success ( (ans1,ans2), tail )
      | Failed s -> Failed s
  | Failed s  -> Failed s

let many p = fun lst ->
  let ans = ref []
  let rec inner stream = 
    match p stream with
    | Success (x, tail) ->
        ans := x :: !ans
        inner tail
    | Failed _ -> Success (List.rev !ans, stream)
  inner lst
  
let (<|>) p1 p2 = fun lst ->
  match p1 lst with
  | Success (a,b) as ans -> ans
  | Failed _ -> 
      match p2 lst with
      | Success _ as ans -> ans
      | Failed _ as ans -> ans



let createParserForwardedToRef () =
    let dummyParser = fun stream -> failwith "a parser created with createParserForwardedToRef was not initialized"
    let r = ref dummyParser
    (fun stream -> !r stream), r : Parser<_,'u> * Parser<_,'u> ref
