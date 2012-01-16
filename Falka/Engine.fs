module Falka.Engine
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

exception YardRule of Yard.Core.IL.Production.t<Yard.Core.IL.Source.t, Yard.Core.IL.Source.t>
exception EvalFail of string * Expr

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

module Printer = Yard.Generators.YardPrinter.Generator
open Printer 

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

let eval : MethodInfo*Expr -> _ = fun (meth,expr) ->
  let matcher e = 
    let error x = raise (EvalFail x)
    // Maybe to use `OK of 'a | Error of exn` instead of exception to be sure that
    // all exception were catched
    let rec inner e : Production.t<_,_> = 
      let error' s = raise (EvalFail (s,e))
      match e with
      | Call (_,mi,args) -> 
        begin
            match mi with
            | GrGrDot -> 
              begin // >>.
                if List.length args <> 2 then error' ">>. should have 2 parameters"
                let (l,r) = List.head args, List.nth args 1
                let (l,r) = inner l, inner r
                // if l or r is PSeq we should apend lists
                // btw, we should convert Production.t to Production.elem if necessary
                let lst : Production.elem<_,_> list = 
                  let wrap (l: Production.t<_,_>)  = 
                    if ILHelper.isPSeq l 
                    then ILHelper.lst_of_PSeq_exn l
                    else [{omit=false; rule=l; binding=None; checker=None}]
                  (wrap l) @ (wrap r)
                Production.PSeq (lst,None)
              end
            | LsBarGr ->
                if List.length args <> 2 then error' "<|> should have 1 parameter"
                let (l,r) = List.head args, List.nth args 1
                let (l,r) = inner l, inner r
                Production.PAlt (l,r)
            | PString -> 
              begin
                if List.length args <> 1 then error' "pstring should have 1 parameter"
                if not (ExprHelper.isValue (List.head args)) then error' "1st arg of pstring should be a value"
                let s = ExprHelper.takeValue (List.head args)
                Production.PLiteral (ILHelper.make_Sourcet s)
              end
            | PChar ->
              begin
                if List.length args <> 1 then error' "pchar should have 1 parameter"
                if not (ExprHelper.isValue (List.head args)) then error' "1st arg of pchar should be a value"
                let s = ExprHelper.takeValue (List.head args)
                Production.PLiteral (ILHelper.make_Sourcet s)
              end
            | _ -> error' "pattern-matching haven't match a part of code"
        end
      | _ -> error' "pattern-matching haven't match a part of code"
    try
      Some (inner e)
    with 
    | EvalFail (s,where) -> None
    | YardRule x -> Some x

  match getBody expr with
    | None -> Printf.printf "\nno body detected\n"; None
    | Some ( (Call (_,mi,args)) as body) -> 
      begin
        Printf.printf "body detected for method %s:\n%s\n\n" meth.Name (body.ToString () )
        let () = 
          match  matcher body with
          | Some x -> 
              Printf.printf "Grammar evaluated!\n"
              let r = ILHelper.makeRule meth.Name x
              let s = Yard.Generators.YardPrinter.Generator.printRule r
              Yard.Generators.YardPrinter.Generator.printTextBox 2 80 s |> Printf.printfn "%s"            
          | None -> Printf.printf "Grammar failed to evaluate\n"
        Some body
      end
    | _ -> Printf.printf "bad body detected\n"; None
  
  
