namespace Yard.Core
  module IL = begin
    module Source = begin
      type t = string * (int * int)
      val toString : string * (int * int) -> string
    end
    module Production = begin
      type IRuleType =
        interface
        end
      type elem<'patt,'expr> =
        {omit: bool;
         rule: t<'patt,'expr>;
         binding: 'patt option;
         checker: 'expr option;}
      and t<'patt,'expr> =
        | PAlt of t<'patt,'expr> * t<'patt,'expr>
        | PSeq of elem<'patt,'expr> list * 'expr option
        | PToken of Source.t
        | PRef of Source.t * 'expr option
        | PMany of t<'patt,'expr>
        | PMetaRef of Source.t * 'expr option * t<'patt,'expr> list
        | PLiteral of Source.t
        | PRepet of t<'patt,'expr> * int option * int option
        | PPerm of t<'patt,'expr> list
        | PSome of t<'patt,'expr>
        | POpt of t<'patt,'expr>
        with
          override ToString : unit -> string
        end
    end
    module Rule = begin
      type t<'patt,'expr> =
        {name: string;
         args: 'patt list;
         body: Production.t<'patt,'expr>;
         _public: bool;
         metaArgs: 'patt list;}
    end
    module Grammar = begin
      type t<'patt,'expr> = Rule.t<'patt,'expr> list
    end
    module Definition = begin
      type info =
        {fileName: string;}
      type t<'patt,'expr> =
        {info: info;
         head: 'expr option;
         grammar: Grammar.t<'patt,'expr>;
         foot: 'expr option;}
      val empty : t<'a,'b>
    end
  end

