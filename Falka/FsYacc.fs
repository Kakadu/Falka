module Falka.FsYacc
open Yard.Generators.FsYaccPrinter
open Printf
open System.IO

let fixFsYaccOutput filenamePrefix nsName =
  let func filename =
    let lines = File.ReadAllLines filename
    let stream = new System.IO.StreamWriter (filename)
    let tokenStart = lines |> Array.tryFindIndex (fun s -> s.StartsWith "type token")
    let tokenEnd = lines |> Array.tryFindIndex (fun s -> s.StartsWith "type tokenId")
    match tokenStart,tokenEnd with
      | (Some x,Some y) when x < y ->
           let f i (s: string) =
               match i with
               | _ when i=x -> stream.WriteLine (sprintf "open %s" nsName)
               | _ when i>x && i<y ->
                  //printfn "line  `%s` was skipped" s
                  ()
               | _ -> stream.WriteLine s
           Array.iteri f lines
           stream.Close ()
      | _ -> failwith "Failed to substitute right token type"          
  func (filenamePrefix + ".fs")
  func (filenamePrefix + ".fsi")

let print (filename: string) tokenType gr =
  let s = Yard.Generators.FsYaccPrinter.Generator.generate2 gr tokenType
  File.WriteAllLines(filename, [s])

open System.Diagnostics

let runFsYacc fsyacccmd moduleName _ filename =
  let args = sprintf "--module %s %s" moduleName filename
  let p = new Process ()
  p.StartInfo.FileName <- fsyacccmd
  p.StartInfo.Arguments <- args
  p.StartInfo.UseShellExecute <- false
  p.StartInfo.RedirectStandardOutput <- true
  Printf.printfn "Executing commmand: `%s %s`..." fsyacccmd filename
  try
    let _ = p.Start ()

    let output = p.StandardOutput.ReadToEnd ()
    p.WaitForExit ()  
    if p.ExitCode<>0
    then 
      printfn "%s\n\n" output
      false
    else
      printfn "FsYacc finised successfully"
      true
  with :? System.ComponentModel.Win32Exception as exn ->    
    printfn "Exception while executing fsyacc: %s" exn.Message
    false