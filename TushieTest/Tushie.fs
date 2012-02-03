module GeneratedParser.Parser
open Microsoft.FSharp.Text.Lexing
open Falka.Comb
open Microsoft.FSharp.Compiler.Reflection

type InnerParser () = class
  inherit Test2.InnerParser ()
  override this.Start (stream: ITokenLexer<_>) =
    let curstream = ref stream
    let tokenizer (lexbuf: LexBuffer<_>) =
      if (!curstream).is_empty ()
      then failwith "fuck"
      else
        let ans = (!curstream).peek ()
        Printf.printfn "ans = %A" ans
        curstream := (!curstream).tail ()
        lexbuf.StartPos <- Position.FirstLine("filename")
        lexbuf.EndPos <- lexbuf.StartPos.EndOfToken(1)
        ans
    let res = GeneratedParser.Yacc.Start tokenizer (LexBuffer<_>.FromString "asdfasfdasdfasdf")
    Success (res, !curstream)

end
