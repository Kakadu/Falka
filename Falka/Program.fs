open System.Reflection
open Parser.Common

let dll = Assembly.LoadFrom @"..\..\..\Test\bin\Debug\Test.dll"
let types = dll.GetTypes ()
let root = dll.GetType "Test"
(* let () = Printf.printf "%s\n" (root.ToString ()) *)

let parserClasses = Array.filter Parser.Common.isParserClass (root.GetMembers ())
    
let iterParserClass (c: MemberInfo) = 
  assert (isParserClass c)
  let f x = 
    match isParserFunction x with
    | Some a -> Some (a,x)
    | None -> None
  let members = c.ReflectedType.GetMembers  ()
  let methods = Array.filter (fun (x: MemberInfo) -> x.MemberType = System.Reflection.MemberTypes.Method) members
  let u = filter_map f methods
  ()
  

let () = Array.iter iterParserClass parserClasses


