module GeneratedParser.Parser
open Microsoft.FSharp.Text.Lexing
open Falka.Comb
open Microsoft.FSharp.Compiler.Reflection

let wrapToken: Test2.token -> GeneratedParser.Yacc.token option = function
  | Test2.OPERATOR x ->  Some (GeneratedParser.Yacc.Operator x)
  | Test2.NUMBER x ->  Some (GeneratedParser.Yacc.Number x)
  | _ -> None

type InnerParser () = class
  inherit Test2.InnerParser ()
  override this.Expression (stream: ITokenLexer<_>) =
    let curstream = ref stream
    let tokenizer (lexbuf: LexBuffer<_>) =
      if (!curstream).is_empty ()
      then failwith "fuck! stream is empty"
      else
        let ans = (!curstream).peek ()
        let ans = 
          match wrapToken ans with
          | Some ans ->
              curstream := (!curstream).tail ()
              ans
          | None     -> GeneratedParser.Yacc.EOF ""
        Printf.printfn "ans = %A" ans
        lexbuf.StartPos <- Position.FirstLine("filename")
        lexbuf.EndPos <- lexbuf.StartPos.EndOfToken(1)
        ans
    try
      let res = GeneratedParser.Yacc.yard_start_1 tokenizer (LexBuffer<_>.FromString "asdfasfdasdfasdf")
      Success (res, !curstream)

    with
      | exn ->
          if exn.Message.Equals("parse error")
          then printfn "stream = %A" stream; Failed "fsyacc raised exception about parse error"
          else
            System.Console.WriteLine(exn.StackTrace)
            raise exn
end
