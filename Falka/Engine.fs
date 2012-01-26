module Falka.Engine
open System.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Yard.Core
open EngineHelpers
open Printf
open Yard.Core.IL

exception YardRule of Production.t<Source.t, Source.t>
exception EvalFail of string * Expr

module Printer = Yard.Generators.YardPrinter.Generator
open Printer 

let error x = raise (EvalFail x)

let eval : MethodInfo*Expr -> _ = fun (meth,expr) ->
  let matcher e = 
    // Maybe to use `OK of 'a | Error of exn` instead of exception to be sure that
    // all exception were catched
    let rec inner e : Production.t<_,_> = 
      let error' s = raise (EvalFail (s,e))
      match e with
      | ThisCall (mi,args) -> 
          Production.PRef (ILHelper.make_Sourcet mi.Name,None)
      | Call (_,mi,args) -> 
        begin
            match mi with
            | DotGrGr     // .>>
            | GrGrDot ->  // >>.
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
            | LsBarGr ->
                if List.length args <> 2 then error' "<|> should have 1 parameter"
                let (l,r) = List.head args, List.nth args 1
                let (l,r) = inner l, inner r
                Production.PAlt (l,r)
            | PString -> 
                if List.length args <> 1 then error' "pstring should have 1 parameter"
                if not (ExprHelper.isValue (List.head args)) then error' "1st arg of pstring should be a value"
                let s = ExprHelper.takeValue (List.head args)
                Production.PLiteral (ILHelper.make_Sourcet s)
            | PChar ->
                if List.length args <> 1 then error' "pchar should have 1 parameter"
                if not (ExprHelper.isValue (List.head args)) then error' "1st arg of pchar should be a value"
                let s = ExprHelper.takeValue (List.head args)
                Production.PLiteral (ILHelper.make_Sourcet s)
            | PMany ->
                if List.length args <> 1 then error' "pmany should have 1 parameter"
                Production.PMany (inner (List.head args))
            | _ -> error' (sprintf "pattern-matching haven't match a part of code (mi.Name = %A)" mi.Name )
        end
      | _ -> error' (sprintf "pattern-matching haven't match a part of code (expr = %A)" e)
    try
      Some (inner e)
    with 
    | EvalFail (s,where) -> 
        Printf.printf "error occured: %s\n" s
        Printf.printfn "%A" where
        None
    | YardRule x -> Some x

  match getBody expr with
    | None -> Printf.printf "\nno body or bad detected:\n%A\n" expr; None
    | Some body -> 
      begin
        //Printf.printf "body detected for method %s:\n%A\n\n" meth.Name body
        match  matcher body with
          | Some x -> 
              Printf.printf "Rule evaluated!\n"
              let r = ILHelper.makeRule meth.Name x
              let s = Yard.Generators.YardPrinter.Generator.printRule r
              Yard.Generators.YardPrinter.Generator.printTextBox 2 80 s |> Printf.printfn "%s"
              Some r
          | None -> Printf.printf "Grammar failed to evaluate\n"; None
      end
    
  
