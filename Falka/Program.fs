open System.Reflection
open Parser.Common

let dll = Assembly.LoadFrom @"Test.dll"
let rootns = dll.GetType "Test"
let _inn : MemberInfo [] = rootns.GetMember "innerParser"
let _inn2 : MemberInfo = (_inn.GetValue 0 :?> MemberInfo)
let _mems2 = _inn2.ReflectedType.GetMembers()
(* (_inn2 as System.RuntimeType).GetMembers()
{System.Reflection.MemberInfo[8]}
    [0]: {Microsoft.FSharp.Core.FSharpFunc`2[FParsec.CharStream`1[System.Object],FParsec.Reply`1[Microsoft.FSharp.Core.Unit]] get_ws()}
    [1]: {a asdf[a](a)}
    [2]: {System.String ToString()}
    [3]: {Boolean Equals(System.Object)}
    [4]: {Int32 GetHashCode()}
    [5]: {System.Type GetType()}
    [6]: {Void .ctor()}
    [7]: {Microsoft.FSharp.Core.FSharpFunc`2[FParsec.CharStream`1[System.Object],FParsec.Reply`1[Microsoft.FSharp.Core.Unit]] ws}
*)
let _props = _inn2.ReflectedType.GetProperties ()

let root = dll.GetType "Test+innerParser"
    
let iterParserClass (c: MemberInfo) = 
  assert (isParserClass c)
  let f x = 
    match isParserFunction x with
    | Some a -> Some (a,x)
    | None -> None
  let members = c.ReflectedType.GetMembers ()
  let props = c.ReflectedType.GetProperties ()
  //let methods = Array.filter (fun (x: MemberInfo) -> x.MemberType = System.Reflection.MemberTypes.Method) members
  let u = filter_map f members
  ()
  
let () = iterParserClass root


