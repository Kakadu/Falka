module Falka.Attributes

type parserStrategy = GLR | LALR | RecDes

type ParserClassAttribute () =
    inherit System.Attribute()

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
    match (acc,x) with
    | (Some _,_) -> acc
    | (_,(:? RecDesAttribute)) -> Some (RecDes)
    | (_,(:? LALRAttribute)) -> Some (LALR)
    | (_,(:? GLRAttribute)) -> Some (GLR)
    | _ -> None
    ) None

let isParserClass (mem: System.Reflection.MemberInfo) = 
  let attrs = mem.GetCustomAttributes false
  mem.ReflectedType.IsClass &&
    (attrs |> Array.exists (fun attr -> attr :? ParserClassAttribute))
  
let filter_map f xs = 
  let acc = ref []
  xs |> Array.iter (fun x ->
    match f x with 
    | Some y -> acc := y :: !acc
    | None -> ()
  )
  acc


type FalkaLexer = 
  abstract member peek: unit -> string
  //abstract member peekNth: int -> string
  abstract member tail: unit -> FalkaLexer
  //abstract member tailN: unit -> FalkaLexer
  
