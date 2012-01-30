module Falka.Comb2

type ITokenLexer<'token> = 
  abstract member is_empty : unit -> bool
  abstract member peek : unit -> 'token
  abstract member tail : unit -> ITokenLexer<'token>

type Result<'a,'token> = 
  | Success of 'a * ITokenLexer<'token>
  | Failed of string

type Parser<'token, 'Result> = ITokenLexer<'token> -> Result<'Result, 'token>

let (>>.) (p1: Parser<'t,'r>) (p2: Parser<'t,'q>) : Parser<'t,'q> = fun lst ->
  match p1 lst with
  | Success (_, tail) -> p2 tail
  | Failed s -> Failed s
  
let (.>>.) (p1: Parser<'t,'r>) (p2: Parser<'t,'u>) : Parser<'t,'r*'u> = fun lst ->
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
  
let (<|>) (p1: Parser<'t,'u>) (p2: Parser<'t,'u>) : Parser<'t,'u> = fun lst ->
  match p1 lst with
  | Success (a,b) as ans -> ans
  | Failed _ -> 
      match p2 lst with
      | Success _ as ans -> ans
      | Failed _ as ans -> ans

let (|>>) (p: Parser<'t,'r>) (f: 'r -> 'b) : Parser<'t,'b> =
  fun s ->
    match p s with
    | Success (ans,tail) -> Success (f ans, tail)
    | Failed s -> Failed s

let pipe2 p1 p2 f =
  fun s ->
    let r1 = p1 s
    match r1 with
    | Success (a, s2) ->
        let r2 = p2 s2
        match r2 with
        | Success (b, s3) -> Success (f a b, s3)
        | Failed _ -> r2
    | Failed _ -> r1

let pipe3 
  (p1:Parser<'t,'a> ) (p2:Parser<'t,'b>) (p3:Parser<'t,'c>) (f:'a -> 'b -> 'c -> 'd) = fun s1 ->
    let r1 = p1 s1
    match r1 with
    | Success (a, s2) ->
        let r2 = p2 s2
        match r2 with
        | Success (b, s3) ->
            let r3 = p3 s3
            match r3 with
            | Success (c, s4) -> Success (f a b c, s4)
            | Failed s -> Failed s
        | Failed s -> Failed s
    | Failed s -> Failed s

let createParserForwardedToRef () =
    let dummyParser = fun stream -> failwith "a parser created with createParserForwardedToRef was not initialized"
    let r = ref dummyParser
    (fun stream -> !r stream), r : Parser<_,'u> * Parser<_,'u> ref
