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
                     _))) -> Some ans
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

module ILHelper = begin
  let isPSeq = function
  | Production.PSeq _ -> true
  | _ -> false
  let lst_of_PSeq_exn = function
  | Production.PSeq (l,_) -> l
  | _ -> failwith "wrong argument of lst_of_PSeq_exn"
  let make_Sourcet s = (s,(0,0))
  let makeRule name body : IL.Rule.t<_,_> =
    { name=name; _public=true; args=[]; body=body; metaArgs=[] }
end
