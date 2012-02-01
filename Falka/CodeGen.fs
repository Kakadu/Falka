module CodeGen
open Printf
open System.IO
open Microsoft.FSharp.Compiler.CodeDom
open System.CodeDom.Compiler
open System.Reflection

let outAssembly = @"Tushie.dll"
let tempFileName = @"Tushie.fs"
let newModule = @"GeneratedParser.Parser"
let referencedAssemblies = 
  ["System.dll"; "FSharp.PowerPack.dll"; "Common.dll"; "Unquote.dll"
  ;@"C:\Program Files\Reference Assemblies\Microsoft\FSharp\2.0\Runtime\v4.0\FSharp.Core.dll"
  ;@"C:\Program Files\FSharpPowerPack-2.0.0.0\bin\FSharp.Compiler.CodeDom.dll"]

let getSource (initialDllName,nsname,classname) =
  let h = new StreamWriter (tempFileName)
  fprintf h "module %s\n" newModule
  fprintf h "type innerParser () = class\n  member this.ololo = 1\nend\n"
  h.Close ()

let compile ((dllname,nsname,classname) as classinfo) (srcFiles: string list) =
  let cparams = new CompilerParameters ()
  cparams.OutputAssembly <- outAssembly
  List.iter (fun x -> ignore (cparams.ReferencedAssemblies.Add x)) (referencedAssemblies @ [dllname])
  cparams.GenerateExecutable <- false
  let fsProvider = new FSharpCodeProvider ()
  getSource classinfo
  let filenames = (tempFileName :: srcFiles) |> List.toArray
  printfn "Executing F# compiler"
  let res = fsProvider.CompileAssemblyFromFile (cparams, filenames)  
  if res.Errors.Count > 0
  then
    printfn "Output of F# compiler"
    for s in res.Errors do
      printfn "%A" s
    printfn "End of output of F# compiler"
    None
  else 
    printfn "Path to assembly: %s" res.PathToAssembly
    Some res.CompiledAssembly


