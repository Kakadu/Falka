// Learn more about F# at http://fsharp.net
open TestRunner
open Falka.Comb

let t = new Test2.innerTokenizer ()
let tokens = 
    run_fparsec t.run "1+2*3" (fun (x,_,_) -> x) 
                (fun (msg,_,_) -> 
                  printfn "Failed tokenization. %s" msg
                  failwith msg)
    |> (fun x -> x @ [Test2.EOF "" ])
let () = printfn "tokens  = %A" tokens

let p = new GeneratedParser.Parser.InnerParser ()
let lexer = new Test2.innerLexer (tokens)
let ans = p.Start lexer
let () = 
    match ans with
    | Success (ans, tail) ->
        printfn "ans = %A" ans
        printfn "tail = %A" tail
    | Failed s -> printfn "Parsing failed: %s\n" s
()