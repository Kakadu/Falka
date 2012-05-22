module TestRunner
open System

(* fparsec *)
open FParsec
let run_fparsec p str ok fail =
    match run p "1+2*3" with
    | Success (x,y,z) -> ok (x,y,z)
    | Failure (x,y,z) -> fail (x,y,z)

open Falka.Comb
let test2 () =
  let t = new Test2.innerTokenizer ()
  let tokens = 
    run_fparsec t.run "1+2*3" (fun (x,_,_) -> x) 
                (fun (msg,_,_) -> 
                  printfn "Failed tokenization. %s" msg
                  failwith msg)
  let p = new Test2.InnerParser ()
  let lexer = new Test2.innerLexer (tokens)
  let dt1 = System.DateTime.Now 
  let ans = p.Expression lexer
  let dt2 = System.DateTime.Now
  let () = 
    match ans with
    | Success (ans, tail) ->
        printfn "ans = %A" ans
        printfn "tail = %A" tail
        let delta = dt2 - dt1
        printfn "delta = %A" delta
    | Failed s -> printfn "Parsing failed: %s\n" s
  ()
let () = test2 ()


(*
open Test3
let test3 () =
  let test1 = "create function fun1 begin select a from b end"
  let test2 = "create function fun1 begin end"
  let test3 = "select a from b "
  let tokens =
    let lexbuf = Lexing.LexBuffer<_>.FromString test1
    let rec getTokens () =
      seq {
        match Lexer.tokens lexbuf with
        | Lexer.EOF _ as ans -> yield ans
        | ans ->
            yield ans
            yield! getTokens ()
      }
    getTokens ()
  ()
  let lst = tokens |> Seq.toList
  printfn "tokens = %A" lst
  let p = new Test3.Parser.InnerParser ()
  let lexer = new Test3.Parser.innerLexer (lst)
  match p.CreateFunction lexer with
  | Success (ans,_) -> printfn "ans = %A" ans
  | Failed s        -> printfn "failed: %A" s

let () = test3 ()

module Codegen =
  open Microsoft.FSharp.Reflection
  open System.Reflection
  open System.IO

  let codegen3 tokenTypeName =
    let nameToUnionCtor (uci:UnionCaseInfo) =
      let fields: PropertyInfo [] = uci.GetFields ()
      (uci.Name, if fields.Length > 0 then Some (fields.[0].PropertyType.FullName) else None)
    let ucis : _ [] =
      FSharpType.GetUnionCases(typeof<Test3.Lexer.token>) |> Array.map nameToUnionCtor
    using (new StreamWriter ("qwe.fs")) (fun h ->
     using (new StreamWriter ("qwe2.fs")) (fun h2 ->
      ucis |> Array.iter (fun (initName, typ) ->
        let typeStr = match typ with
                      | Some "System.String" -> "string"
                      | Some x-> x
                      | None -> "unit"
        let name = initName.ToLower ()
        fprintfn h "  member this.%s (): Result<%s,%s> =" name typeStr tokenTypeName
        fprintfn h "    let o = this :> ITokenLexer<%s>" tokenTypeName
        fprintfn h "    if o.is_empty ()"
        fprintfn h "    then Failed \"input is empty\""
        fprintfn h "    else "
        fprintfn h "      let el = o.peek()"
        fprintfn h "      match el with"
        fprintfn h "         | %s x -> Success (x, o.tail())" initName
        fprintfn h "         | _    -> Failed (sprintf \"cant parse %s. %%A is on then top\" el)" initName
        fprintfn h ""
        let combName = initName.ToLower () |> (fun (s:string) ->
          s.ToCharArray () |> (fun (arr: _ []) -> arr.[0] <- Char.ToUpper arr.[0]; new String(arr) )
        )
        fprintfn h2 "  [<LexerCombinator(\"%s\",\"%s\")>]" initName typeStr
        fprintfn h2 "  member this.%s stream : Result<%s, token> =" combName typeStr
        fprintfn h2 "    (stream?%s : unit -> Result<%s,token>) ()" name typeStr
      )
     )
    )
 
//let _ = Codegen.codegen3 "Lexer.token"
let test4 () =
  let t = new Test4.innerTokenizer ()
  let tokens =
    run_fparsec t.run "1+2*3" (fun (x,_,_) -> x)
                (fun (msg,_,_) ->
                  printfn "Failed tokenization. %s" msg
                  failwith msg)
  let tokens = tokens @ [Test4.EOF ""]
  let p = new Test4.InnerParser ()
  let lexer = new Test4.innerLexer (tokens)
  let ans = p.Start2_call lexer
  let () =
    match ans with
    | Success (ans, tail) ->
        printfn "ans = %A" ans
        printfn "tail = %A" tail
    | Failed s -> printfn "Parsing failed: %s\n" s
  ()

let () = test4 ()
*)