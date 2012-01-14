module TestRunner
(*open Test
open Falka.Comb
open Falka.Common

type ListLexer (tokens: string list) =
  interface FalkaLexer with
    member this.peek () = List.head tokens
    member this.tail () = (new ListLexer (List.tail tokens)) :> FalkaLexer

let () = 
  let p = new innerParser ()
  let l = new ListLexer(["1";"+";"2";"-";"3";"*";"3";"/";"3"])
  match p.expr l with
  | Failed -> Printf.eprintf "failed\n"
  | Parsed (_,s) -> Printf.printf "pasrsed\n" 
  ()
  *)

(* fparsec *)
open FParsec
let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let () = 
  let c = new Test.parser1 ()
  let _ = test c.number "1.2 "
  let _ = test c.floatlist "[1.2,3.5]"
  let _ = test c.expr "1+2+3+4"
  ()