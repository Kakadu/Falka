module Falka.FsYacc
open Yard.Generators.FsYaccPrinter
open Printf

let print filename tokentype gr =
  let s = Yard.Generators.FsYaccPrinter.Generator.generate gr tokentype
  System.IO.File.WriteAllLines(filename, [s])

open System.Diagnostics
let fsyacccmd = @"C:\Program Files\FSharpPowerPack-2.0.0.0\bin\fsyacc.exe"
let runFsYacc moduleName openModule filename =
  let args = sprintf "--module %s --open %s %s" moduleName openModule filename
  let p = new Process ()
  p.StartInfo.FileName <- fsyacccmd
  p.StartInfo.Arguments <- args
  p.StartInfo.UseShellExecute <- false
  p.StartInfo.RedirectStandardOutput <- true
  Printf.printfn "Executing commmand: `%s %s`..." fsyacccmd filename
  let _ = p.Start ()

  let output = p.StandardOutput.ReadToEnd ()
  p.WaitForExit ()
  printfn "FsYacc finised."
  printfn "%s\n\n" output
