(**
 * This code analyzes input DLL and bodies of parser-functions,
 * builds Yard grammar and executes FsYacc
 *)

module Falka.Main

open Falka
open Printf
open System.Reflection
open Falka.Attributes
open Falka.Utils
open Microsoft.FSharp.Quotations

let (dllname,   (* DLL where parser located *)
     nsname,    (* Namespace where to look for parser *) 
     classname, (* class with parser functions *)
     workdir    (* where to put generated files *) ) = 
  (ref @"Test.dll", ref @"Test2", ref @"InnerParser", ref @"..\..\..\HybridTest1")
  //(ref @"Test.dll", ref @"Test3.Parser", ref @"InnerParser", ref @"..\..\..\HybridTest2")
let outYaccFilePrefix = "HybridParser"
let doCompilation = false
let fsyacccmd = ref @"C:\Program Files\FSharpPowerPack-2.0.0.0\bin\fsyacc.exe"

let () = 
  let specs = 
    ["--dll",    ArgType.String ((:=)dllname), "filename of DLL";
     "--ns",     ArgType.String ((:=)nsname),  "Namespace where to look for parser class";
     "--class",  ArgType.String ((:=)classname),  "parser class's name";
     "--pwd"  ,  ArgType.String ((:=)workdir),  "where to put generated files";
     "--fsyacc", ArgType.String ((:=)fsyacccmd), "fsyacc executable"
    ] |> List.map (fun (sh, ty, desc) -> ArgInfo(sh, ty, desc))
  ArgParser.Parse(specs, fun s -> printfn "wrong argument %s" s)

let (innerParser: System.Type, parserAttribute) =
  let dll = Assembly.LoadFrom !dllname
  let rootns = dll.GetType !nsname
  let _inn : MemberInfo [] = rootns.GetMember !classname
  let parser = _inn.GetValue 0 :?> System.Type
  match isParserClass parser with
  | Some attr -> (parser, attr)
  | None -> failwith (sprintf "It seems that %s has no start Rule defined\n" parser.Name)

let (startRuleName, tokenNamespace, extraNamespaces) =
  ( parserAttribute.StartRuleName
  , parserAttribute.TokenNamespace
  , parserAttribute.ExtraNamespaces)
let methods =
  // TODO: look at properties too.
  // NB. We cannot appply ReflectedDefinition Attribute to properrties
  let g arr = 
    arr 
    |> Array.toList
    |> List.filter isParserFunction
  let m' = innerParser.GetMethods () |> g
  m' |> List.filter_map (fun m  ->
    match Expr.TryGetReflectedDefinition m with
    | None -> None
    | Some body -> Some (m,body) 
  )

let tokensData = 
    innerParser.GetMethods () |> Array.filter_map (fun mi ->
      mi |> isLexerCombinatorFunction |> Option.map (fun x -> (mi,x))
    )

let getTokeType,isTokenRule =
  let isTokenRule s =
    tokensData
      |> Array.find_opt (fun (mi: MethodInfo,_) -> mi.Name.Equals(s)) 
      |> Option.map (fun (mi,_) -> mi.Name)
  let getTokenType s =
    tokensData
    |> Array.find_opt (fun (mi,_) -> mi.Name.Equals(s))
    |> Option.map (fun (_,attr) -> attr.TokenType)
  (getTokenType, isTokenRule)

let show_parserfun : (MethodInfo * Expr) -> _ = fun (info,expr) ->
  let name = info.Name
  Printf.sprintf "Name: %s\nBody:\n%s\n" name (expr.ToString ())

let loglines : seq<string> ref = ref Seq.empty
let () =
  let lst = List.map show_parserfun methods |> List.toSeq
  loglines := lst

open EngineHelpers

let opens = 
  Seq.append 
    extraNamespaces
    [ tokenNamespace
      ; !nsname
      ; "Microsoft.FSharp.Quotations"
      ; "Microsoft.FSharp.Quotations.Patterns"
      ; "Microsoft.FSharp.Compiler"
      ; "Microsoft.FSharp.Compiler.Reflection"
    ] |> seq

let () = System.IO.Directory.SetCurrentDirectory !workdir

let (yaccStartRuleName, usedTokens) =
  let rules = List.map (Engine.eval startRuleName isTokenRule) methods
  let rules = List.filter_map (fun x -> x) rules
  let rules = 
    let expander = new Yard.Core.Convertions.ExpandBrackets.ExpandBrackets ()
    expander.ConvertList rules
  let rules = 
    let eofer = new Yard.Core.Convertions.AddEOF.AddEOF ()
    eofer.ConvertList rules
  let usedRuleNames = Yard.Core.Checkers.reachableRulesInfo_of_list rules
  printfn "rules = %A" (rules |> List.map (fun r -> r.name))
  printfn "usedRuleNames = %A" usedRuleNames
  let rules = 
    rules |> List.filter (fun r -> usedRuleNames |> List.exists (fun name -> name = r.name))
  let usedTokens = Yard.Generators.FsYaccPrinter.Generator.findTokens rules
  let headtext = sprintf "\nopen %s\n" (String.concat "\nopen " opens)
  let definition = ILHelper.makeDefinition rules "filename" (Some headtext)
  Printf.printfn "\nGrammar is:"
  let () = Yard.Generators.YardPrinter.Generator.generate definition 
            |> (fun s ->
                  Printf.printfn "%s" s
                  System.IO.File.WriteAllLines("gr.yrd", [s])
               )
  let startRuleName = 
    let startRule = definition.grammar |> List.tryFind (fun rule -> rule._public)
    match startRule with
    | Some x -> x.name
    | None -> failwith "this grammar has not start rule"
  let tokenTyper (tname:string) =
    match getTokeType tname with
    | Some x -> x
    | None  -> failwith (sprintf "Token type is not specified for token %s" tname)
  FsYacc.print (outYaccFilePrefix + ".fsy") tokenTyper definition
  (startRuleName,usedTokens)

let () =
  System.IO.File.WriteAllLines(@"log.txt", !loglines)

let evalNewAssembly (asm: Assembly) = 
  let fullName = CodeGen.newModule + "+InnerParser"
  let p = asm.GetType fullName
  p.GetMembers () |> Array.map (fun x -> x.ToString()) |> Array.iter (fun x-> printfn "%A" x)
  printfn "Ended"

let () =
  if FsYacc.runFsYacc !fsyacccmd "GeneratedParser.Yacc" nsname (outYaccFilePrefix + ".fsy")
  then
    let rules2kill = []
    //TODO: rules to kill are such rules which are used inside of startRule
    CodeGen.getSource (!nsname,!classname) (startRuleName,yaccStartRuleName) rules2kill usedTokens (tokenNamespace,tokensData)
    if doCompilation
    then
      match CodeGen.compile (!dllname,nsname,classname) ["asdf.fsi"; "asdf.fs"; CodeGen.tempFileName] with
      | Some x when x <> null -> evalNewAssembly x
      | _ -> printfn "Failed to compile generated parser"
    else ()
  else
    printfn "Error while executing FsYacc"


