module Falka.Attributes

type parserStrategy = GLR | LALR | RecDes

type ParserClassAttribute (start: string) =
    inherit System.Attribute()
    member this.StartRuleName = start

type ParserFunctionAttribute () =
    inherit System.Attribute()

type GLRAttribute () = 
    inherit ParserFunctionAttribute()

type LALRAttribute () = 
    inherit ParserFunctionAttribute()

type RecDesAttribute () = 
    inherit ParserFunctionAttribute()

let isParserFunction (mem: System.Reflection.MemberInfo) = 
  let attrs = mem.GetCustomAttributes false
  attrs |> Array.fold (fun acc x -> 
    acc || 
    (match x with
    | :? ParserFunctionAttribute -> true
    | _        -> false)
  ) false

(*  attrs |> Array.fold (fun acc x -> 
    match (acc,x) with
    | (Some _,_) -> acc
    | (_,(:? RecDesAttribute)) -> Some (RecDes)
    | (_,(:? LALRAttribute)) -> Some (LALR)
    | (_,(:? GLRAttribute)) -> Some (GLR)
    | _ -> None
    ) None
*)
open Falka.Utils
let isParserClass (mem: System.Reflection.MemberInfo) =
  if not mem.ReflectedType.IsClass
  then None
  else
    let attrs  = mem.GetCustomAttributes false
    let f (x: obj) =
      match x with
        | :? ParserClassAttribute -> Some ((x :?> ParserClassAttribute).StartRuleName)
        | _  -> None
    let ans = Array.filter_map f attrs
    match ans with
    | [| |] -> None
    | [| name |] -> Some name
    | _ ->
       printfn "It seems that class %s has more than 1 ParserClassAttribute" mem.Name
       None
  
let filter_map f xs = 
  let acc = ref []
  xs |> Array.iter (fun x ->
    match f x with
    | Some y -> acc := y :: !acc
    | None -> ()
  )
  acc

