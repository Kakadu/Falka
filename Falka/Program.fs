module Falka.Main
open Falka

let (dllname, nsname, classname) = 
  //(@"Test.dll", @"Test", @"parser1")
  (@"Test.dll", @"Test2", @"innerParser")
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
let () =
  let rules = List.map (Engine.eval startRuleName tokenRuleNames) methods
  let rules = List.filter_map (fun x -> x) rules
  let definition = ILHelper.makeDefinition rules "filename"
  Printf.printfn "\nGrammar is:"
  let () = 
    Yard.Generators.YardPrinter.Generator.generate definition 
    |> (fun s ->
          Printf.printfn "%s" s
          System.IO.File.WriteAllLines("gr.yrd", [s])
       )
  FsYacc.print "asdf.fsy" "Token.token" definition
  ()

let () =
  System.IO.File.WriteAllLines(@"log.txt", !loglines)

let () =
  FsYacc.runFsYacc "asdf.fsy"

let _ = System.Console.ReadKey ()