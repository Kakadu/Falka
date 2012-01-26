module Falka.Main
open Falka

let (dllname, nsname, classname) = 
  //(@"Test.dll", @"Test", @"parser1")
  (@"Test.dll", @"Test2", @"innerParser")
open System.Reflection
open Falka.Attributes
open Falka.Utils
open Microsoft.FSharp.Quotations

let dll = Assembly.LoadFrom dllname
let innerParser : System.Type = 
  let rootns = dll.GetType nsname
  let _inn : MemberInfo [] = rootns.GetMember classname
  (_inn.GetValue 0 :?> System.Type)

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
  
let () = 
  let lst = List.map show_parserfun methods |> List.toSeq
  System.IO.File.WriteAllLines(@"log.txt", lst)

open EngineHelpers
let () = 
  let rules = List.map Engine.eval methods
  let rules = List.filter_map (fun x -> x) rules
  let definition = ILHelper.makeDefinition rules "filename"
  Printf.printfn "\nGrammar is:"
  Yard.Generators.YardPrinter.Generator.generate definition |> Printf.printfn "%s"
  FsYacc.print "asdf.fsy" definition
  ()

