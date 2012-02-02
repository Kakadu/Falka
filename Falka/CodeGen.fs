module CodeGen
open Printf
open System
open System.IO
open Microsoft.FSharp.Compiler.CodeDom
open System.CodeDom.Compiler
open System.Reflection

let outAssembly = @"Tushie.dll"
let tempFileName = @"Tushie.fs"
let newModule = @"GeneratedParser.Parser"

let referencedAssemblies = 
  ["System.dll"; "System.Numerics.dll"; "FSharp.PowerPack.dll"; "FalkaCommon.dll"
  ;"FParsec.dll"; "FParsecCS.dll"
  ;@"C:\Program Files\Reference Assemblies\Microsoft\FSharp\2.0\Runtime\v4.0\FSharp.Core.dll"
  ;@"C:\Program Files\FSharpPowerPack-2.0.0.0\bin\FSharp.Compiler.CodeDom.dll"]

let getSource (nsname,classname) startRule killedRules =
  printfn "Generating Source: startRule=%s, killed rules = %A" startRule killedRules
  let h = new StreamWriter (tempFileName)
  fprintfn h "module %s" newModule
  fprintfn h "open Microsoft.FSharp.Text.Lexing"
  fprintfn h "open Falka.Comb"
  fprintfn h "open Microsoft.FSharp.Compiler.Reflection"
  fprintfn h ""
  fprintfn h "type InnerParser () = class"
  fprintfn h "  inherit %s.%s ()" nsname classname
  fprintfn h "  override this.%s (stream: ITokenLexer<_>) =" startRule
  fprintfn h "    let lexbuf = ref (LexBuffer<_>.FromString \"asdfasfdasdfasdf\")"
  fprintfn h "    let curstream = ref stream"
  fprintfn h "    let tokenizer (lexbuf: LexBuffer<_>) ="
  fprintfn h "      if (!curstream).is_empty ()"
  fprintfn h "      then failwith \"fuck\""
  fprintfn h "      else"
  fprintfn h "        let ans = (!curstream).peek ()"
  fprintfn h "        curstream := (!curstream).tail ()"
  fprintfn h "        lexbuf.StartPos <- Position.FirstLine(\"%s\")" "filename" 
  fprintfn h "        lexbuf.EndPos <- new Position(\"%s\",0,0,String.length ans?Item)" "filename"
  fprintfn h "        ans"
  fprintfn h "    let res = GeneratedParser.Yacc.%s tokenizer lexbuf" startRule
  fprintfn h "    Success (res, !curstream)\n"
  fprintfn h "end"
  h.Close ()

let compile ((dllname,nsname,classname) as classinfo) (srcFiles: string list) =
  let cparams = new CompilerParameters ()
  cparams.OutputAssembly <- outAssembly
  cparams.IncludeDebugInformation <- true
  List.iter (fun x -> ignore (cparams.ReferencedAssemblies.Add x)) (referencedAssemblies @ [dllname])
  cparams.GenerateExecutable <- false
  let fsProvider = new FSharpCodeProvider ()
  let filenames = srcFiles |> List.toArray
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


