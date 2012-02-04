module TestRunner
open System

(* fparsec *)
open FParsec
let run_fparsec p str ok fail =
    match run p "1+2*3" with
    | Success (x,y,z) -> ok (x,y,z)
    | Failure (x,y,z) -> fail (x,y,z)

open Falka.Comb
let test2 () =
  let t = new Test2.innerTokenizer ()
  let tokens = 
    run_fparsec t.run "1+2*3" (fun (x,_,_) -> x) 
                (fun (msg,_,_) -> 
                  printfn "Failed tokenization. %s" msg
                  failwith msg)
  let p = new Test2.InnerParser ()
  let lexer = new Test2.innerLexer (tokens)
  let ans = p.Expression lexer
  let () = 
    match ans with
    | Success (ans, tail) ->
        printfn "ans = %A" ans
        printfn "tail = %A" tail
    | Failed s -> printfn "Parsing failed: %s\n" s
  ()

let test3 () =
  let tokens =
    run_fparsec Test3.Tokenizer.start "select a from b" (fun (x,_,_) -> x) 
                (fun (msg,_,_) -> 
                  printfn "Failed tokenization. %s" msg
                  failwith msg)
  let tokens = tokens @ [Test3.EOF ""]
  printfn "tokens = %A" tokens

let () = test3 ()
