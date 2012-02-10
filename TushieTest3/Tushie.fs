module GeneratedParser.Parser
open Microsoft.FSharp.Text.Lexing
open Falka.Comb
open Microsoft.FSharp.Compiler.Reflection

let wrapToken: Test3.Lexer.token -> GeneratedParser.Yacc.token option = function
  | Test3.Lexer.KW_SELECT x ->  Some (GeneratedParser.Yacc.Kw_select x)
  | Test3.Lexer.KW_FROM x ->  Some (GeneratedParser.Yacc.Kw_from x)
  | Test3.Lexer.LPAREN x ->  Some (GeneratedParser.Yacc.Lparen x)
  | Test3.Lexer.RPAREN x ->  Some (GeneratedParser.Yacc.Rparen x)
  | Test3.Lexer.GLOBALVAR x ->  Some (GeneratedParser.Yacc.Globalvar x)
  | Test3.Lexer.LOCALVAR x ->  Some (GeneratedParser.Yacc.Localvar x)
  | Test3.Lexer.IDENT x ->  Some (GeneratedParser.Yacc.Ident x)
  | _ -> None

type InnerParser () = class
  inherit Test3.Parser.InnerParser ()
  override this.SqlExpression (stream: ITokenLexer<_>) =
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
