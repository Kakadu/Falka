open System.Reflection
open Parser.Common
open Parser.Utils
open Microsoft.FSharp.Quotations

let dll = Assembly.LoadFrom @"Test.dll"
let innerParser : System.Type = 
  let rootns = dll.GetType "Test"
  let _inn : MemberInfo [] = rootns.GetMember "innerParser"
  (_inn.GetValue 0 :?> System.Type)

let (methods,_) =  // TODO: look at properties too.
  let f x = match isParserFunction x with
    | Some y -> Some (x,y)
    | None -> None
  let g arr = 
    arr 
    |> Array.toList
    |> List.filter_map f
  let (m',_) = (innerParser.GetMethods () |> g, innerParser.GetProperties () |> g)
  (m' 
   |> List.filter_map (fun (m,sort)  ->
    match Expr.TryGetReflectedDefinition m with
    | None -> None
    | Some body -> Some (m,body,sort) 
  ),1)


let () = ()
let () = ()
let () = ()

