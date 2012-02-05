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
  let ans = p.Expression lexer
  let () = 
    match ans with
    | Success (ans, tail) ->
        printfn "ans = %A" ans
        printfn "tail = %A" tail
    | Failed s -> printfn "Parsing failed: %s\n" s
  ()

open Test3
let test3 () =
  let tokens =
    let lexbuf = Lexing.LexBuffer<_>.FromString "select a from b"
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
  //let tokens = Seq.toList tokens
  //printfn "tokens = %A" tokens
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
        fprintfn h "  member this.%s : Result<%s,%s> =" name typeStr tokenTypeName
        fprintfn h "    let o = this :> ITokenLexer<%s>" tokenTypeName
        fprintfn h "    if o.is_empty ()"
        fprintfn h "    then Failed \"input is empty\""
        fprintfn h "    else match o.peek ()  with"
        fprintfn h "         | %s x -> Success (x, o.tail())" initName
        fprintfn h "         | _    -> Failed \"cant parse %s\"" initName
        fprintfn h ""
        let combName = initName.ToLower () |> (fun (s:string) ->
          s.ToCharArray () |> (fun (arr: _ []) -> arr.[0] <- Char.ToUpper arr.[0]; new String(arr) )
        )
        fprintfn h2 "  [<LexerCombinator(\"%s\",\"%s\")>]" initName tokenTypeName
        fprintfn h2 "  member this.%s stream : Result<%s, token> =" combName typeStr
        fprintfn h2 "    (stream?operator : unit -> Result<%s,token>) ()" typeStr
      )
     )
    )
 
let _ = Codegen.codegen3 "Lexer.token"
