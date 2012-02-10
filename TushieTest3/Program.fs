open Test3
open Falka.Comb

let test3 () =
  let test1 = "create function fun1 begin select a from b end"
  let test2 = "create function fun1 begin end"
  let test3 = "create function fun1 select a from b"
  let test4 = "select a from b "
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
  let p = new GeneratedParser.Parser.InnerParser ()
  let lexer = new Test3.Parser.innerLexer (lst)
  match p.CreateFunction lexer with
  | Success (ans,_) -> printfn "Succed: result = %A" ans
  | Failed s        -> printfn "failed: %A" s

let () = test3 ()

let _ = System.Console.ReadLine()
