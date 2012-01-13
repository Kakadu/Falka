module Falka.Comb

type res<'a> = 
  | Parsed of 'a * Falka.Common.FalkaLexer
  | Failed

let (>>>) p1 p2 l = 
  match p1 l with
  | Failed -> Failed
  | Parsed (_,ll) -> p2 ll

let (>>=) p1 f s =
  match p1 s with
  | Failed -> Failed
  | Parsed (ans,s) -> f ans s

let p_many p s = 
  let rec loop s =
    match p s with
    | Failed -> Parsed((),s)
    | Parsed(_,s)-> loop s
  loop s

let (|||) p1 p2 s = 
  let ans = p1 s
  match ans with
  | Failed -> p2 s
  | _ -> ans