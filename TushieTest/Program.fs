// Learn more about F# at http://fsharp.net
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
  let n = 10
  for i=1 to n do
    ans <-  ans + "+" + ans
  done
  printfn "%i" ans.Length
  ans
  

let t = new Test2.innerTokenizer ()
let tokens = 
    printfn "input length = %d" (String.length input)
   // printfn ".............= %s" input.[0..20]
    run_fparsec t.run input 
                (fun (x,y,z) -> x) 
                (fun (msg,_,_) -> 
                  printfn "Failed tokenization. %s" msg
                  failwith msg)
    |> (fun x -> x @ [Test2.EOF "" ]) 
    |> (fun lst -> ResizeArray.ofList lst)

//let () = printfn "tokens  = %A" tokens
let () = 
  printfn "tokens count = %d" tokens.Count
  ()
// Generated
let gen2 () =
  let p = new GeneratedParser.Parser.InnerParser ()
  let lexer = new Test2.innerLexer (0,tokens)
  //let watch = new System.Diagnostics.Stopwatch ()
  //let () = watch.Start ()
  let dt1 = System.DateTime.Now 
  let ans = p.Start lexer
  //let () = watch.Stop ()
  let dt2 = System.DateTime.Now 
  (dt2-dt1,ans)

let pureFsYacc () = ()
// Combinators
let gen1 () = 
  let p = new Test2.InnerParser ()
  let lexer = new Test2.innerLexer (0,tokens)
  //let watch = new System.Diagnostics.Stopwatch ()
  let dt1 = System.DateTime.Now 
  let ans = p.Expression lexer
  let dt2 = System.DateTime.Now 
  (dt2-dt1,ans)

//let () = System.GC.AddMemoryPressure((int64)500000)

open FParsec
let gen3 () = 
  let watch = new System.Diagnostics.Stopwatch ()
  let dt1 = System.DateTime.Now  
  let ans = 
    match run Test2FParsec.expr input with
      | Success (x,y,z) as exn -> 
          //let t = sprintf "%A %A %A %A" exn x y z
          x
      | (Failure (x,y,z)) as exn-> 
          //let _ = sprintf "%A %A %A %A" exn x y z
          failwith "Fparsec failed"
  let dt2 = System.DateTime.Now
  (dt2-dt1,ans)

let eval (f: unit -> System.TimeSpan * _) n =
  let _,_ = f ()
  let sumtime: int64 ref = ref (0L)
  let rec loop n = 
    if n=0L then ()
    else
      //let () = System.GC.Collect ()
      let time,_ = f ()
      printfn "time = %A" time
      sumtime := !sumtime + int64(time.Milliseconds)
      loop (n-1L)
  let () = loop n
  !sumtime/n

let () = 
  let count = 2L
  let x1 = int(eval gen1 count)
  let x2 = 1//int(eval gen2 count)
  let x3 = 1//int(eval gen3 count)
  printfn "My Combinators:    %A" x1
  printfn "Generated:         %A" x2
  printfn "FParsec:           %A" x3  
  printfn "comb/generated.    %f" (float(x1) / float(x2))
  printfn "generated/FParsec. %f" (float(x2) / float(x3))

///let _ = System.Console.ReadLine ()