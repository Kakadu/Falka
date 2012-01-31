module EngineHelpers

open System.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Yard.Core

let getBody x =  
  match x with
  | Lambda 
     (_
     ,Lambda (_,
                Let (_,
                     (Call (a,b,c) as ans),
                     _))) -> Some ans   // member this.asdf = ... >>. ...
  | Lambda 
     (_
     ,Lambda (_,
                Let (_
                    ,(Lambda(_
                           ,Call (a,b,c)
                           ) as ans)
                    ,_))) -> Some ans // member this.comb1 = this.comb2
  | _ -> None

let (|GrGrDot|_|) (e: MethodInfo) = 
  match e with
  | _ when e.Name = "op_GreaterGreaterDot" -> Some GrGrDot
  | _ -> None
let (|DotGrGr|_|) (e: MethodInfo) =
  match e with
  | _ when e.Name = "op_DotGreaterGreater" -> Some DotGrGr
  | _ -> None
let (|PString|_|) (e:MethodInfo) =
  match e with
  | _ when e.Name = "pstring" -> Some PString
  | _ -> None
let (|PChar|_|) (e:MethodInfo) =
  match e with
  | _ when e.Name = "pchar" -> Some PChar
  | _ -> None
let (|LsBarGr|_|) (e:MethodInfo) =
  match e with
  | _ when e.Name = "op_LessBarGreater" -> Some LsBarGr
  | _ -> None
let (|PMany|_|) (e:MethodInfo) =
  match e with
  | _ when e.Name = "many" -> Some PMany
  | _ -> None
let (|PPipe3|_|) (e:MethodInfo) =
  match e with
  | _ when e.Name = "pipe3" -> Some PPipe3
  | _ -> None
let (|PBarGrGr|_|) (e:MethodInfo) =
  match e with
  | _ when e.Name = "op_BarGreaterGreater" -> Some PBarGrGr
  | _ -> None

let (|ThisCall|_|) (e:Expr) =
  match e with
    | Lambda(_arg
            ,Call (_obj,mi,args) 
            ) -> Some (mi,args)
    | _ -> None

open Yard.Core.IL

module ExprHelper = begin
  let isValue = function
  | Value _ -> true
  | _ -> false
  let takeValue (y: Expr) = 
    match y with
    | Value (x,_) -> // in our case x always be a string
        x.ToString ()
    | _ -> failwith "Wrong argument of takeValue"
end
open IL
module ILHelper = begin
  let isPSeq = function
  | Production.PSeq _ -> true
  | _ -> false
  let lst_of_PSeq_exn = function
  | Production.PSeq (l,_) -> l
  | _ -> failwith "wrong argument of lst_of_PSeq_exn"
  let make_Sourcet s: Source.t = (s,(0,0))
  let makeRule name body isStartRule : IL.Rule.t<_,_> =
    { name=name; _public=isStartRule; args=[]; body=body; metaArgs=[] }

  let makeDefinition rules fileName (h: string option): Definition.t<Source.t,Source.t> =
    let h =
      match h with
      | Some x -> Some (make_Sourcet x)
      | None -> None
    {info = {fileName=fileName};  head=h; foot=None; grammar=rules }

  let makeElem b rule omit checker : IL.Production.elem<Source.t,Source.t> =
    { rule=rule; omit=omit; checker=checker; binding=b }
end

open System
let makeIdentFunc () =
  let next = [| 'a'; 'a'; 'a' |]
  let last = next.Length - 1
  let incr_char (x:char) = x |> Convert.ToInt32 |> ((+)1) |> Convert.ToChar
  let rec incr_ident i =
      if i<0
      then failwith "problem in identifiers generation"
      if next.[i] = 'z'
      then next.[i] <- 'a'; incr_ident (i-1)
      else next.[i] <- incr_char next.[i]
  (fun () ->
    if next.ToString () = "zzz"
    then failwith "problem in identifiers generation"
    else
      let ans = new String(next)
      incr_ident last
      ans
  )
