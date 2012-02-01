module Falka.Main
open Falka

let (dllname, nsname, classname) = 
  //(@"Test.dll", @"Test", @"parser1")
  (@"Test.dll", @"Test2", @"InnerParser")
open System.Reflection
open Falka.Attributes
open Falka.Utils
open Microsoft.FSharp.Quotations

let (innerParser: System.Type, startRuleName, tokenType, tokenRuleNames) =
  let dll = Assembly.LoadFrom dllname
  let rootns = dll.GetType nsname
  let _inn : MemberInfo [] = rootns.GetMember classname
  let parser = _inn.GetValue 0 :?> System.Type
  match isParserClass parser with
  | Some attr -> (parser, attr.StartRuleName, attr.TokenType, attr.Tokens)
  | None -> failwith (sprintf "It seems that %s has no start Rule defined\n" parser.Name)

let methods =
  // TODO: look at properties too.
  // NB. We cannot appply ReflectedDefinition Attribute to properrties
  let g arr = 
    arr 
    |> Array.toList
    |> List.filter isParserFunction
  let m' = innerParser.GetMethods () |> g
  m' |> List.filter_map (fun m  ->
    match Expr.TryGetReflectedDefinition m with
    | None -> None
    | Some body -> Some (m,body) 
  )

let show_parserfun : (MethodInfo * Expr) -> _ = fun (info,expr) ->
  let name = info.Name
  Printf.sprintf "Name: %s\nBody:\n%s\n" name (expr.ToString ())

let loglines : seq<string> ref = ref Seq.empty
let () =
  let lst = List.map show_parserfun methods |> List.toSeq
  loglines := lst

open EngineHelpers

let opens = 
    [ nsname
      ; "Microsoft.FSharp.Quotations"
      ; "Microsoft.FSharp.Quotations.Patterns"
      ; "Microsoft.FSharp.Compiler"
      ; "Microsoft.FSharp.Compiler.Reflection"
    ] |> seq
let () =
  let rules = List.map (Engine.eval startRuleName tokenRuleNames) methods
  let rules = List.filter_map (fun x -> x) rules  
  let headtext = sprintf "\nopen %s\n" (String.concat "\nopen " opens)
                     
  let definition = ILHelper.makeDefinition rules "filename" (Some headtext)
  Printf.printfn "\nGrammar is:"
  let () = 
    Yard.Generators.YardPrinter.Generator.generate definition 
    |> (fun s ->
          Printf.printfn "%s" s
          System.IO.File.WriteAllLines("gr.yrd", [s])
       )
  FsYacc.print "asdf.fsy" "Test2.token" definition
  ()

let () =
  System.IO.File.WriteAllLines(@"log.txt", !loglines)

let evalNewAssembly (asm: Assembly) = 
  let fullName = CodeGen.newModule + "+InnerParser"
  let p = asm.GetType fullName
  p.GetMembers () |> Array.map (fun x -> x.ToString()) |> Array.iter (fun x-> printfn "%A" x)
  printfn "Ended"
  ()

let () =
  if FsYacc.runFsYacc "GeneratedParser.Yacc" nsname "asdf.fsy"
  then
    match CodeGen.compile (dllname,nsname,classname) ["asdf.fsi"; "asdf.fs"] with
    | Some x when x <> null -> evalNewAssembly x
    | _ -> printfn "Failed to compile new class"
  else
    printfn "Error while executing FsYacc"

//let _ = System.Console.ReadKey ()
