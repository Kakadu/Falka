// Learn more about F# at http://fsharp.net
open TestRunner
open Falka.Comb

let input = 
  let mutable ans = "1+2*3"
  let n = 5
  for i=1 to 5 do
    ans <-  ans + "+" + ans
  done
  ans
  

let t = new Test2.innerTokenizer ()
let tokens = 
    run_fparsec t.run input (fun (x,_,_) -> x) 
                (fun (msg,_,_) -> 
                  printfn "Failed tokenization. %s" msg
                  failwith msg)
    |> (fun x -> x @ [Test2.EOF "" ])
//let () = printfn "tokens  = %A" tokens
// Generated
let gen2 () =
  let p = new GeneratedParser.Parser.InnerParser ()
  let lexer = new Test2.innerLexer (tokens)
  let watch = new System.Diagnostics.Stopwatch ()
  let () = watch.Start ()
  let ans = p.Start lexer
  let () = watch.Stop ()
  let dt2 = System.DateTime.Now 
  (watch.ElapsedTicks,ans)
// Combinators
let gen1 () = 
  let p = new Test2.InnerParser ()
  let lexer = new Test2.innerLexer (tokens)
  let watch = new System.Diagnostics.Stopwatch ()
  let () = watch.Start ()
  let ans = p.Expression lexer
  let () = watch.Stop ()
  (watch.ElapsedTicks,ans)

let () = System.GC.AddMemoryPressure((int64)500000)

open FParsec
let gen3 () = 
  let watch = new System.Diagnostics.Stopwatch ()
  let () = watch.Start ()
  let () = 
    match run Test2FParsec.expr input with
      | Success (_,_,_) -> ()
      | Failure _       -> printfn "Fparsec failed"
  let () = watch.Stop ()
  (watch.ElapsedTicks,null)

let eval (f: unit -> int64 * _) n =
  let _,_ = f ()
  let sumtime: int64 ref = ref (0L)
  let rec loop n = 
    if n=0L then ()
    else
      let () = System.GC.Collect ()
      let time,_ = f ()
      //printfn "time = %A" time
      sumtime := !sumtime + time
      loop (n-1L)
  let () = loop n
  !sumtime/n



let () = 
  let count = 1000L
  let x1,x2,x3 = int(eval gen1 count), int(eval gen2 count), int(eval gen3 count)
  printfn "My Combinators:    %A" x1
  printfn "Generated:         %A" x2
  printfn "FParsec:           %A" x3  
  printfn "comb/generated.    %f" (float(x1) / float(x2))
  printfn "generated/FParsec. %f" (float(x2) / float(x3))

(*  
let () = 
    match ans with
    | Success (ans, tail) ->
        printfn "ans = %A" ans
        printfn "tail = %A" tail
        //printfn "delta = %A" (dt2-dt1)
    | Failed s -> printfn "Parsing failed: %s\n" s
()
*)


///let _ = System.Console.ReadLine ()