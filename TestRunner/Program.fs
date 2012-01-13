open Test
open Falka.Comb
open Falka.Common

type ListLexer (tokens: string list) =
  interface FalkaLexer with
    member this.peek () = List.head tokens
    member this.tail () = (new ListLexer (List.tail tokens)) :> FalkaLexer

let () = 
  let p = new innerParser ()
  let l = new ListLexer(["1";"+";"2";"-";"3"])
  match p.expr l with
  | Failed -> Printf.eprintf "failed\n"
  | Parsed (_,s) -> Printf.printf "pasrsed\n" 
  ()