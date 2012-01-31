module CodeGen
open Printf
open System.IO
open Microsoft.FSharp.Compiler.CodeDom
open System.CodeDom.Compiler
open System.Reflection

let outAssembly = "Tushie.dll"
let tempFileName = "Tushie.fs"

let getSource (initialDllName,nsname,classname) =
  let h = new StreamWriter (tempFileName)
  fprintf h "module Asdf\ntype innerParser () = class\n  member this.ololo = 1\nend\n" 
  h.Close ()

let compile ((dllname,nsname,classname) as classinfo) =
  let cparams = new CompilerParameters ()
  cparams.OutputAssembly <- outAssembly
  let _ = cparams.ReferencedAssemblies.Add "System.dll"
  let _ = cparams.ReferencedAssemblies.Add dllname
  cparams.GenerateExecutable <- false
  let fsProvider = new FSharpCodeProvider ()
  getSource classinfo
  printfn "Executing F# compiler"
  let res = fsProvider.CompileAssemblyFromFile (cparams, tempFileName)
  if res.Errors.Count >0 
  then
    printfn "Output of F# compiler"
    for s in res.Errors do
      printfn "%A" s
    printfn "End of output of F# compiler"
    None
  else 
    printfn "Path to assembly: %s" res.PathToAssembly
    Some res.CompiledAssembly


