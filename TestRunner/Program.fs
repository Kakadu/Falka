module TestRunner


(* fparsec *)
open FParsec
let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let () = 
  let c = new Test.parser1 ()
(*  let _ = test c.number "1.2 "
  let _ = test c.floatlist "[1.2,3.5]" *)
  let _ = test c.expr "1+2+3+4" 
  ()
let run_fparsec p str ok fail =
    match run p "1+2*3" with
    | Success (x,y,z) -> ok (x,y,z)
    | Failure (x,y,z) -> fail (x,y,z)

open Falka.Comb
let () =
  let t = new Test2.innerTokenizer ()
  let tokens = 
    run_fparsec t.run "1+2*3" (fun (x,_,_) -> x) 
                (fun (msg,_,_) -> 
                  printfn "Failed tokenization. %s" msg
                  failwith msg)
  let p = new Test2.innerParser ()
  let lexer = new Test2.innerLexer (tokens)
  let ans = p.expr lexer
  let () = 
    match ans with
    | Success (ans, tail) ->
        printfn "ans = %A" ans
        printfn "tail = %A" tail
    | Failed s -> printfn "Parsing failed: %s\n" s
  ()
