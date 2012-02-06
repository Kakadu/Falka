module Falka.Main
open Falka
open Printf
open System.Reflection
open Falka.Attributes
open Falka.Utils
open Microsoft.FSharp.Quotations

let (dllname, nsname, classname, workdir) = 
  //(@"Test.dll", @"Test", @"parser1")
  (@"Test.dll", @"Test2", @"InnerParser", @"..\..\..\TushieTest")
  //(@"Test.dll", @"Test3", @"InnerParser", @"..\..\..\TushieTest3")
let doescompile = false
let (innerParser: System.Type, startRuleName, tokenType) =
  let dll = Assembly.LoadFrom dllname
  let rootns = dll.GetType nsname
  let _inn : MemberInfo [] = rootns.GetMember classname
  let parser = _inn.GetValue 0 :?> System.Type
  match isParserClass parser with
  | Some attr -> (parser, attr.StartRuleName, attr.TokensType)
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

let getTokeType,isTokenRule =
  let lst =
    innerParser.GetMethods () |> Array.filter_map (fun mi ->
      mi |> isLexerCombinatorFunction |> Option.map (fun x -> (mi,x))
    )
  let isTokenRule s =
    lst |> Array.exists (fun (_,attr:LexerCombinatorAttribute) -> attr.TokenName.Equals(s))
  let getTokenType s =
    lst
    |> Array.find_opt (fun (mi,_) -> mi.Name.Equals(s))
    |> Option.map (fun (_,attr) -> attr.TokenType)
  (getTokenType, isTokenRule)

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

let () = System.IO.Directory.SetCurrentDirectory workdir
let () =
  let rules = List.map (Engine.eval startRuleName isTokenRule) methods
  let rules = List.filter_map (fun x -> x) rules
  let expander = new Yard.Core.Convertions.ExpandBrackets.ExpandBrackets ()
  let rules = expander.ConvertList rules
  (*let eofer = new Yard.Core.Convertions.AddEOF.AddEOF ()
  let rules = eofer.ConvertList rules *)
  let headtext = sprintf "\nopen %s\n" (String.concat "\nopen " opens)
  let definition = ILHelper.makeDefinition rules "filename" (Some headtext)
  Printf.printfn "\nGrammar is:"
  let () = Yard.Generators.YardPrinter.Generator.generate definition 
            |> (fun s ->
                  Printf.printfn "%s" s
                  System.IO.File.WriteAllLines("gr.yrd", [s])
               )
  let tokenTyper (tname:string) =
    match getTokeType tname with
    | Some x -> x
    | None  -> failwith (sprintf "Token type is not specified for token %s" tname)
  FsYacc.print "asdf.fsy" tokenTyper definition

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
    FsYacc.fixFsYaccOutput "asdf" nsname
    let rules2kill = []
    //TODO: rules to kill are such rules which are used inside of startRule
    CodeGen.getSource (nsname,classname) startRuleName rules2kill
    if doescompile
    then
      match CodeGen.compile (dllname,nsname,classname) ["asdf.fsi"; "asdf.fs"; CodeGen.tempFileName] with
      | Some x when x <> null -> evalNewAssembly x
      | _ -> printfn "Failed to compile new class"
    else ()
  else
    printfn "Error while executing FsYacc"

//let _ = System.Console.ReadKey ()
