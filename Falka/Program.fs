open System.Reflection
open Parser.Common

let dll = Assembly.LoadFrom @"..\..\..\Test\bin\Debug\Test.dll"
let types = dll.GetTypes ()
let root = dll.GetType "Test+innerParser"
    
let iterParserClass (c: MemberInfo) = 
  assert (isParserClass c)
  let f x = 
    match isParserFunction x with
    | Some a -> Some (a,x)
    | None -> None
  let members = c.ReflectedType.GetMembers ()
  //let methods = Array.filter (fun (x: MemberInfo) -> x.MemberType = System.Reflection.MemberTypes.Method) members
  let u = filter_map f members
  ()
  
let () = iterParserClass root


