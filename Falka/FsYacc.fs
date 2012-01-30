﻿module Falka.FsYacc
open Yard.Generators.FsYaccPrinter
open Printf

let print filename tokentype gr =
  let s = Yard.Generators.FsYaccPrinter.Generator.generate gr tokentype
  System.IO.File.WriteAllLines(filename, [s])

open System.Diagnostics
let fsyacccmd = @"D:\Program Files\FSharpPowerPack-2.0.0.0\bin\fsyacc.exe"
let runFsYacc filename =
  let p = new Process ()
  p.StartInfo.FileName <- fsyacccmd
  p.StartInfo.Arguments <- filename
  p.StartInfo.UseShellExecute <- false
  p.StartInfo.RedirectStandardOutput <- true
  Printf.printfn "Executing commmand: `%s %s`..." fsyacccmd filename
  let _ = p.Start ()

  let output = p.StandardOutput.ReadToEnd ()
  p.WaitForExit ()
  printfn "FsYacc finised."
  printfn "%s\n\n" output
