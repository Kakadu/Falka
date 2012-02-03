module Falka.Engine
open System.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Yard.Core
open EngineHelpers
open Printf
open Yard.Core.IL
open Swensen
open Falka.Utils

exception YardRule of Production.t<Source.t, Source.t>
exception EvalFail of string * Expr

module Printer = Yard.Generators.YardPrinter.Generator
open Printer 

let decompile f = f |> Unquote.Operators.reduce |> Unquote.Operators.decompile
let error x = raise (EvalFail x)
let nextIdent = EngineHelpers.makeIdentFunc ()

let eval startRuleName (tokenRuleNames: string []) (meth: MethodInfo,expr: Expr) =
  let matcher e = 
    // Maybe to use `OK of 'a | Error of exn` instead of exception to be sure that
    // all exception were catched
    let rec inner e : Production.t<Source.t,Source.t> =
      let error' s = raise (EvalFail (s,e))
      match e with
      | ThisCall (mi,args) -> 
          // TODO: maybe we should patch FsYaccGenerator to explain generator which 
          // names we should use for tokens and rule names (afair in grammar name 
          // NUMBER is associated with Lexer's T_NUMBER variant.
          if Array.exists (fun x -> x.Equals mi.Name) tokenRuleNames
          then Production.PToken (ILHelper.make_Sourcet mi.Name)
          else Production.PRef (ILHelper.make_Sourcet mi.Name,None)
      | Call (_,mi,args) -> 
        begin
            match mi with
            | DotGrGr ->  // .>>
                if List.length args <> 2 then error' ".>> should have 2 parameters"
                let (l,r) = List.head args, List.nth args 1
                let (l,r) = inner l, inner r
                let right_bind = nextIdent () |> ILHelper.make_Sourcet |> (fun x -> Some x)
                let (l,r) = (ILHelper.makeElem right_bind l false None
                            ,ILHelper.makeElem None r false None)
                Production.PSeq ([l;r], right_bind)
            | GrGrDot ->  // >>.
                if List.length args <> 2 then error' ">>. should have 2 parameters"
                let (l,r) = List.head args, List.nth args 1
                let (l,r) = inner l, inner r
                let right_bind = nextIdent () |> ILHelper.make_Sourcet |> (fun x -> Some x)
                let (l,r) = (ILHelper.makeElem None l false None
                            ,ILHelper.makeElem right_bind r false None)
                Production.PSeq ([l;r], right_bind)
            | LsBarGr -> // <|>
                if List.length args <> 2 then error' "d should have 1 parameter"
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
            | PPipe3 ->
               match args with
               | [p1;p2;p3;f] ->
                   let idents = List.init 3 (fun _ -> nextIdent ())
                   let bindings = List.map ILHelper.make_Sourcet idents
                   let prods = [inner p1; inner p2; inner p3]
                   let content = 
                     List.map2 (fun bind body -> 
                       EngineHelpers.ILHelper.makeElem (Some bind) body false None) bindings prods
                   let code =
                     match idents with
                     | [a;b;c] -> sprintf "(%s) %s %s %s" (decompile f) a b c
                     | _       -> failwith "Bug in generation action code for pipe3"
                   Production.PSeq (content,Some (ILHelper.make_Sourcet code) )
               | _ -> error' "pipe3 should have 4 parameter"
            | PBarGrGr -> // |>>
               match args with
               | [p;f] -> 
                   // TODO: Mybe we should match return value of `inner p`
                   // and modify second element of tuple if 1st element matches PSeq
                   let ident = nextIdent ()
                   let elem = ILHelper.makeElem (Some (ILHelper.make_Sourcet ident)) (inner p) false None
                   let code = sprintf "(%s) %s" (decompile f) ident
                   Production.PSeq ([elem], Some (ILHelper.make_Sourcet code))
               | _ -> error' "|>> should have 2 parameter"
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
              let isStartRule = (meth.Name = startRuleName)
              let r = ILHelper.makeRule meth.Name x isStartRule
              let s = Yard.Generators.YardPrinter.Generator.printRule r
              Yard.Generators.YardPrinter.Generator.printTextBox 2 80 s |> Printf.printfn "%s"
              Some r
          | None -> Printf.printf "Grammar failed to evaluate\n"; None
      end
    
  
