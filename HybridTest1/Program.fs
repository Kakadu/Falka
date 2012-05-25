﻿(*
 * Testing arithmetic generated parser here.
 * all files in this project except current are autogenerated.
 *) 

module TestRunner = 
  open FParsec
  let run_fparsec p str ok fail =
    match run p str with
    | Success (x,y,z) -> ok (x,y,z)
    | Failure (x,y,z) -> fail (x,y,z)

open TestRunner

open Falka.Comb

let input = 
  let mutable ans = "9+8*3+1*4"
  let n = 9
  for i=1 to n do
    ans <-  ans + "+" + ans
  done
  printfn "%i" ans.Length
  ans
  

let t = new Test2.innerTokenizer ()
let tokens = 
    printfn "input length = %d" (String.length input)
    run_fparsec t.run input 
                (fun (x,y,z) -> x) 
                (fun (msg,_,_) -> 
                  printfn "Failed tokenization. %s" msg
                  failwith msg)
    |> (fun x -> x @ [Test2.EOF "" ]) 
    |> (fun lst -> ResizeArray.ofList lst)

let () = 
  printfn "tokens count = %d" tokens.Count
  ()
// Generated
let gen2 () =
  let p = new GeneratedParser.Parser.InnerParser ()
  let lexer = new Test2.innerLexer (0,tokens)
  #if stopwatch
  let watch = new System.Diagnostics.Stopwatch ()
  let () = watch.Start ()
  #else 
  let dt1 = System.DateTime.Now 
  #endif
  let ans = p.Start lexer
  #if stopwatch
  let () = watch.Stop ()
  (float watch.ElapsedMilliseconds, ans)
  #else 
  let dt2 = System.DateTime.Now 
  ((dt2-dt1).TotalMilliseconds,ans)
  #endif

// Combinators
let gen1 () = 
  let p = new Test2.InnerParser ()
  let lexer = new Test2.innerLexer (0,tokens)  
  #if stopwatch
  let watch = new System.Diagnostics.Stopwatch ()
  let () = watch.Start ()
  #else 
  let dt1 = System.DateTime.Now 
  #endif
  let ans = p.Expression lexer
  #if stopwatch
  let () = watch.Stop ()
  (float watch.ElapsedMilliseconds, ans)
  #else 
  let dt2 = System.DateTime.Now 
  ((dt2-dt1).TotalMilliseconds,ans)
  #endif

open FParsec
let gen3 () = 
  #if stopwatch
  let watch = new System.Diagnostics.Stopwatch ()
  let () = watch.Start ()
  #else 
  let dt1 = System.DateTime.Now 
  #endif
  let ans = 
    match run Test2FParsec.expr input with
      | Success (x,y,z) as exn -> 
          //let t = sprintf "%A %A %A %A" exn x y z
          x
      | (Failure (x,y,z)) as exn -> 
          //let _ = sprintf "%A %A %A %A" exn x y z
          failwith "Fparsec failed"
  #if stopwatch
  let () = watch.Stop ()
  (float watch.ElapsedMilliseconds, ans)
  #else 
  let dt2 = System.DateTime.Now 
  ((dt2-dt1).TotalMilliseconds,ans)
  #endif

let eval (f: unit -> float * _) n =
  let _,_ = f ()
  let sumtime: float ref = ref 0.0
  let rec loop n = 
    if n=0 then ()
    else
      let time,_ = f ()      
      printfn "time = %4.5f" time
      stdout.Flush ()
      sumtime := !sumtime + time
      loop (n-1)
  let () = loop n
  !sumtime / float n

let () = 
  let count = 1
  let heatting = 
    for i=1 to 1 do
      ignore (gen1 ())
    done
  let x1 = eval gen1 count
  let x2 = eval gen2 count
  let x3 = eval gen3 count
  printfn "My Combinators:    %A" x1
  printfn "Generated:         %A" x2
  printfn "FParsec:           %A" x3  
  printfn "comb/generated.    %f" (x1 / x2)
  printfn "generated/FParsec. %f" (x2 / x3)

///let _ = System.Console.ReadLine ()