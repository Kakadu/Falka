module Falka.Main
open Falka
open Printf
open System.Reflection
open Falka.Attributes
open Falka.Utils
open Microsoft.FSharp.Quotations

let (dllname, nsname, classname, workdir) = 
  //(@"Test.dll", @"Test", @"parser1")
  //(@"Test.dll", @"Test2", @"InnerParser", @"..\..\..\TushieTest")
  (@"Test.dll", @"Test3.Parser", @"InnerParser", @"..\..\..\TushieTest3")
let doescompile = false
let (innerParser: System.Type, parserAttribute) =
  let dll = Assembly.LoadFrom dllname
  let rootns = dll.GetType nsname
  let _inn : MemberInfo [] = rootns.GetMember classname
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
      //|> Option.map (fun (_,x) -> x.TokenName)
      |> Option.map (fun (mi,_) -> mi.Name)
  let getTokenType s =
    // if token = A of string ans s=="A" returns `string`
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
      ; nsname
      ; "Microsoft.FSharp.Quotations"
      ; "Microsoft.FSharp.Quotations.Patterns"
      ; "Microsoft.FSharp.Compiler"
      ; "Microsoft.FSharp.Compiler.Reflection"
    ] |> seq

let () = System.IO.Directory.SetCurrentDirectory workdir

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
  FsYacc.print "asdf.fsy" tokenTyper definition
  (startRuleName,usedTokens)

let () =
  System.IO.File.WriteAllLines(@"log.txt", !loglines)

let evalNewAssembly (asm: Assembly) = 
  let fullName = CodeGen.newModule + "+InnerParser"
  let p = asm.GetType fullName
  p.GetMembers () |> Array.map (fun x -> x.ToString()) |> Array.iter (fun x-> printfn "%A" x)
  printfn "Ended"
  ()

let () =
  if FsYacc.runFsYacc "GeneratedParser.Yacc" nsname "asdf.fsy"
  then
    let rules2kill = []
    //TODO: rules to kill are such rules which are used inside of startRule
    CodeGen.getSource (nsname,classname) (startRuleName,yaccStartRuleName) rules2kill usedTokens (tokenNamespace,tokensData)
    if doescompile
    then
      match CodeGen.compile (dllname,nsname,classname) ["asdf.fsi"; "asdf.fs"; CodeGen.tempFileName] with
      | Some x when x <> null -> evalNewAssembly x
      | _ -> printfn "Failed to compile new class"
    else ()
  else
    printfn "Error while executing FsYacc"

//let _ = System.Console.ReadKey ()
