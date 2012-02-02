module Falka.Attributes
open Falka.Utils

type parserStrategy = GLR | LALR | RecDes

type ParserClassAttribute (start: string, tokenstype:System.Type, tokens: string) =
    inherit System.Attribute()
    member this.StartRuleName = start
    member this.Tokens = tokens.Split [| ',' |]
    member this.TokensType = tokenstype

type LexerCombinatorAttribute(tokenName: string, tokenType: string) =
    inherit System.Attribute()
    member this.TokenName = tokenName
    member this.TokenType = tokenType

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

let isLexerCombinatorFunction (mem: System.Reflection.MemberInfo) = 
  let attrs = mem.GetCustomAttributes false
  attrs |> Array.fold (fun acc x -> 
    match (acc,x) with
    | (Some _,_) -> acc
    | (None, :? LexerCombinatorAttribute) -> Some (x :?> LexerCombinatorAttribute)
    | _ -> acc
  ) None

let isParserClass (mem: System.Reflection.MemberInfo) =
  if not mem.ReflectedType.IsClass
  then None
  else
    let attrs  = mem.GetCustomAttributes false
    let f (x: obj) =
      match x with
        | :? ParserClassAttribute -> Some (x :?> ParserClassAttribute)
        | _  -> None
    let ans = Array.filter_map f attrs
    match ans with
    | [| |] -> None
    | [| attr |] -> Some attr
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

