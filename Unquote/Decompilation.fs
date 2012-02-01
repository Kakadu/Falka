﻿(*
Copyright 2011 Stephen Swensen

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*)

module internal Swensen.Unquote.Decompilation
open System
open System.Reflection
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations

module P = Microsoft.FSharp.Quotations.Patterns
module DP = Microsoft.FSharp.Quotations.DerivedPatterns

open Swensen.Utils
module EP = Swensen.Unquote.ExtraPatterns
module ER = Swensen.Unquote.ExtraReflection
module OP = Swensen.Unquote.OperatorPrecedence
type OP = OP.OperatorPrecedence

module CustomContext =
    let Zero = (OP(0),OP.Non)

module CC = CustomContext
      
//todo:
//  precedence applied to lhs of . not right, see skipped SourceOpTests
//  note: Dictionary<_,_> values are not sprinted as nicely as in FSI, consider using FSI style
let decompile expr =
    let rec decompile (contextOP,contextAssoc) expr =
        let applyParens = OP.applyParensForPrecInContext contextOP contextAssoc

        match expr with
        | P.Sequential(P.Sequential(lhs, DP.Unit), rhs) ->
            //due to quirky nested structure which handles implicit unit return values
            //need to hack precedence / application of parenthisizes.  we give
            //lhs anecdotally higher precedence context of 10.
            applyParens OP.Semicolon (sprintf "%s; %s" (decompile (OP(10), OP.Non) lhs) (decompile (OP.Semicolon, OP.Right) rhs))
        | P.Sequential(lhs, rhs) -> 
            applyParens OP.Semicolon (sprintf "%s; %s" (decompile (OP.Semicolon, OP.Left) lhs) (decompile (OP.Semicolon, OP.Right) rhs))
        | P.Application(curry, last) -> //application of arguments to a lambda
            applyParens OP.Application (sprintf "%s %s" (decompile (OP.Application, OP.Left) curry) (decompile (OP.Application, OP.Right) last))
        //issue 25 and issue 23: the following "re-sugars" both partially applied and unapplied lambda call expressions
        //must come before Lambdas
        | EP.IncompleteLambdaCall(target, mi, args) -> //assume lambdas are only part of modules.
            match EP.binaryOps |> Map.tryFind mi.Name with
            | Some(symbol,_) -> 
                let sprintedSymbol = 
                    if symbol.StartsWith("*") || symbol.EndsWith("*") then
                        sprintf "( %s )" symbol
                    else
                        sprintf "(%s)" symbol
                match args.Length with
                | 1 -> applyParens OP.Application (sprintf "%s %s" sprintedSymbol (decompileCurriedArgs args))
                | 0 -> sprintedSymbol
                | _ -> failwithf "partial applied binary op should only have 0 or 1 args but has more: %A" args
            | None ->
                match EP.unaryOps |> Map.tryFind mi.Name with
                | Some(symbol) -> sprintf "(~%s)" symbol
                | None -> 
                    let sprintFunction (mi:MethodInfo) =
                        if ER.isOpenModule mi.DeclaringType then ER.sourceName mi
                        else
                            let decompiledTarget =
                                match target with
                                | Some(target) -> (decompile (OP.Dot,OP.Left) target) //instance
                                | None -> ER.sourceName mi.DeclaringType 
                            sprintf "%s.%s" decompiledTarget (ER.sourceName mi)
                    if args.Length = 0 then sprintFunction mi //not sure what precedence should be
                    else applyParens OP.Application (sprintf "%s %s" (sprintFunction mi) (decompileCurriedArgs args))
        | DP.Lambdas(vars, body) -> //addresses issue 27
            let sprintSingleVar (var:Var) = if var.Type = typeof<Unit> then "()" else var.Name
            let sprintedVars =
                vars
                |> List.map  
                    (function 
                        | [var] -> sprintSingleVar var 
                        | tupledVars -> sprintf "(%s)" (tupledVars |> List.map sprintSingleVar |> String.concat ", "))
                |> String.concat " "
            applyParens OP.Fun (sprintf "fun %s -> %s" sprintedVars (decompile CC.Zero body))
        | EP.BinaryInfixCall((symbol, prec), lhs, rhs) -> //must come before Call pattern
            let lhsValue, rhsValue = decompile (prec,OP.Left) lhs, decompile (prec,OP.Right) rhs
            applyParens prec (sprintf "%s %s %s" lhsValue symbol rhsValue)
        | EP.UnaryPrefixCall(symbol, arg) -> //must come before Call pattern
            applyParens OP.PrefixOps (sprintf "%s%s" symbol (decompile (OP.PrefixOps,OP.Non) arg))
        | P.Call(None, mi, [lhs]) when mi.Name = "TypeTestGeneric" ->
            //thinking about making decompile depend on Reduce.isReduced: 
            //so that when lhs |> isReduced, print type info for lhs (since would be helpful here)
            //but I think the sprinting of lhs it is reduced conveys type info sufficiently enough
            applyParens OP.TypeTest (sprintf "%s :? %s" (decompile (OP.TypeTest,OP.Left) lhs) (ER.sprintSig (mi.GetGenericArguments().[0])))
        | P.TypeTest(lhs, ty) -> //seems to be same as TypeTestGeneric
            applyParens OP.TypeTest (sprintf "%s :? %s" (decompile (OP.TypeTest,OP.Left) lhs) (ER.sprintSig ty))
        | EP.Range(startToken,endToken,a,b) -> //not sure about precedence for op ranges
            sprintf "%s%s..%s%s" startToken (decompile CC.Zero a) (decompile CC.Zero b) endToken
        | EP.RangeStep(startToken,endToken,a,b,c) ->
            sprintf "%s%s..%s..%s%s" startToken (decompile CC.Zero a) (decompile CC.Zero b) (decompile CC.Zero c) endToken
        | P.Call(None, mi, target::[]) when mi.DeclaringType.Name = "IntrinsicFunctions" && mi.Name = "UnboxGeneric" -> //i.e. :?>
            let ty = mi.GetGenericArguments().[0]
            applyParens OP.DynamicCast (sprintf "%s :?> %s" (decompile (OP.DynamicCast,OP.Left) target) (ER.sprintSig ty))
        | P.Call(None, mi, target::args) when mi.DeclaringType.Name = "IntrinsicFunctions" -> //e.g. GetChar, GetArray, GetArray2D
            applyParens OP.Dot (sprintf "%s.[%s]" (decompile (OP.Dot, OP.Left) target) (decompileTupledArgs args)) //not sure what precedence is
        | P.Call(target, (ER.FunctionOrGenericValue(fOrGV) as mi), args) -> //instance or static call representing an F# function or generic value
            //if mi has generic args which can't be infered, need to sprint them.
            //if mi takes no arguments, then need to decompile "()", unless mi is an F# value, in which case we omit ()
            let sprintedArgs = 
                sprintf "%s%s"
                    (if ER.genericArgsInferable mi then "" else ER.sprintGenericArgs mi) 
                    (if args.Length = 0 then 
                        match fOrGV with
                        | ER.GenericValue -> ""
                        | ER.Function -> "()"
                     else " " + decompileCurriedArgs args)
            
            let methodName = ER.sourceName mi    
            if ER.isOpenModule mi.DeclaringType then 
                applyParens OP.Application (sprintf "%s%s" methodName sprintedArgs)
            else 
                let decompiledTarget =
                    match target with
                    | Some(target) -> (decompile (OP.Dot,OP.Left) target) //instance
                    | None -> ER.sourceName mi.DeclaringType

                applyParens OP.Application (sprintf "%s.%s%s" decompiledTarget methodName sprintedArgs)
        | P.Call(target, mi, args) -> //a "normal" .net instance or static call
            let decompiledTarget =
                match target with
                | Some(target) -> (decompile (OP.Dot,OP.Left) target) //instance
                | None -> mi.DeclaringType.Name
            applyParens OP.MethodCall (sprintf "%s.%s%s(%s)" decompiledTarget mi.Name (ER.sprintGenericArgsIfNotInferable mi) (decompileTupledArgs args))
        | P.PropertyGet(Some(target), pi, args) -> //instance get
            match pi.Name, args with
            | Regex.Compiled.Match(@"^Item(\d*)?$") _, _ when pi.DeclaringType |> FSharpType.IsUnion ->
                //for UnionCaseTypeTests, require a op_Dynamic implementation
                sprintf "(%s?%s : %s)" (decompile (OP.Dot,OP.Left) target) pi.Name (pi.PropertyType |> ER.sprintSig)
            | _, [] -> sprintf "%s.%s" (decompile (OP.Dot,OP.Left) target) pi.Name //also includes "Item" with zero args
            | "Item", _ -> sprintf "%s.[%s]" (decompile (OP.Dot,OP.Left) target) (decompileTupledArgs args)
            | _, _ -> applyParens OP.MethodCall (sprintf "%s.%s(%s)" (decompile (OP.Dot,OP.Left) target) pi.Name (decompileTupledArgs args))
        | P.PropertyGet(None, pi, args) -> //static get (note: can't accept params)
            let sprintedName =
                if ER.isOpenModule pi.DeclaringType then 
                    sprintf "%s" pi.Name
                else
                    sprintf "%s.%s" pi.DeclaringType.Name pi.Name

            if args.Length = 0 then sprintedName
            else applyParens OP.MethodCall (sprintf "%s(%s)" sprintedName (decompileTupledArgs args))
        | P.PropertySet(target, pi, piArgs, rhs) ->
            let lhs = //leverage PropertyGet sprinting
                match target with
                | Some(instance) -> Expr.PropertyGet(instance, pi, piArgs)
                | None -> Expr.PropertyGet(pi, piArgs)
            //don't know what precedence is
            applyParens OP.LessThanOp (sprintf "%s <- %s" (decompile CC.Zero lhs) (decompile CC.Zero rhs))
        | P.FieldGet(Some(target), fi) ->
            applyParens OP.Dot (sprintf "%s.%s" (decompile (OP.Dot,OP.Left) target) fi.Name)
        | P.FieldGet(None, fi) ->
            applyParens OP.Dot (sprintf "%s.%s" fi.DeclaringType.Name fi.Name)
        | P.FieldSet(target, fi, rhs) ->
            let lhs = //leverage FieldGet sprinting
                match target with
                | Some(instance) -> Expr.FieldGet(instance, fi) 
                | None -> Expr.FieldGet(fi)
            //don't know what precedence is
            applyParens OP.LessThanOp (sprintf "%s <- %s" (decompile CC.Zero lhs) (decompile CC.Zero rhs))
        | DP.Unit -> "()" //must come before Value pattern
        | P.Value(o, _) ->
            match o with
            | null -> "null"
            | _ -> sprintf "%A" o
        | P.NewTuple(args) -> //tuples have at least two elements
            args |> decompileTupledArgs |> sprintf "(%s)" //what is precedence? 10?
        | P.NewArray(_,args) ->
            args |> decompileSequencedArgs |> sprintf "[|%s|]"
        //list union cases more complex than normal union cases since need to consider
        //both cons infix operator and literal list constructions.
        | P.NewUnionCase(uci,args) when uci |> ER.isListUnionCase ->
            if args = [] then
                "[]"
            else
                let rec isLiteralConstruction = function
                    | P.NewUnionCase(_, lhs::(P.NewUnionCase(_, []))::[]) -> true //e.g. _::_::...::[]
                    | P.NewUnionCase(_, lhs::rhs::[]) ->
                        match rhs with
                        | P.NewUnionCase _ -> isLiteralConstruction rhs //e.g. _::_::...
                        | _ -> false //e.g. _::_::x
                    | _ -> failwith "unexpected list union case"

                if expr |> isLiteralConstruction then
                    let rec sprintLiteralConstructionArgs = function
                        | P.NewUnionCase(_, lhs::(P.NewUnionCase(_, []))::[]) -> decompile (OP.Semicolon,OP.Non) lhs
                        | P.NewUnionCase(_, lhs::rhs::[]) ->
                            sprintf "%s; %s" (decompile (OP.Semicolon,OP.Non) lhs) (sprintLiteralConstructionArgs rhs)
                        | _ -> failwith "unexpected list union case"
                    sprintf "[%s]" (sprintLiteralConstructionArgs expr)
                else 
                    //would like to optimize somehow so isLiteralConstruction is not called with every recursive 
                    //decompile of non literal constructions.
                    match args with
                    | lhs::rhs::[] -> applyParens OP.Cons (sprintf "%s::%s" (decompile (OP.Cons,OP.Left) lhs) (decompile (OP.Cons,OP.Right) rhs))
                    | _ -> failwithf "unexpected list union case: %A" expr
        | P.NewUnionCase(uci,args) -> //"typical union case construction"
            match args with
            | [] -> uci.Name
            | _ -> sprintf "%s(%s)" uci.Name (decompileTupledArgs args)
        | P.NewObject(ci, args) ->
            applyParens OP.Application (sprintf "new %s(%s)" (ER.sprintSig ci.DeclaringType) (decompileTupledArgs args))

//            if typeof<System.IDisposable>.IsAssignableFrom(ci.DeclaringType) then
//                //not sure what precedence is
//                applyParens OP.Application (sprintf "new %s(%s)" (ER.sprintSig ci.DeclaringType) (decompileTupledArgs args))
//            else
//                applyParens OP.MethodCall (sprintf "%s(%s)" (ER.sprintSig ci.DeclaringType) (decompileTupledArgs args))
        | P.Coerce(target, _) ->
            //don't even "mention" anything about the coersion (pass through context)
            decompile (contextOP,contextAssoc) target
        | EP.TupleLet(vars, e1, e2) ->
            //if any are mutable, they are all mutable
            let anyMutable = vars |> List.exists (function | Some(v) -> v.IsMutable | None -> false)
            let varNames = vars |> List.map (function | Some(v) -> v.Name | None -> "_")
            applyParens OP.Let (sprintf "let%s%s = %s in %s" (if anyMutable then " mutable " else " ") (varNames |> String.concat ", ") (decompile CC.Zero e1) (decompile CC.Zero e2))
        | P.LetRecursive((firstVar, firstBody)::rest, finalBody) -> //let recursives always have at least thef first var and body
            //note: single line recursive ("and") let bindings are only valid with #light "off", see: http://stackoverflow.com/questions/6501378/what-is-the-non-light-syntax-for-recursive-let-bindings
            let rec decompileRest = function
                | (var:Var, body)::rest ->
                    sprintf " and %s = %s%s" var.Name (decompile CC.Zero body) (decompileRest rest)
                | [] -> sprintf " in %s" (decompile CC.Zero finalBody)
            applyParens OP.Let (sprintf "let rec %s = %s%s" firstVar.Name (decompile CC.Zero firstBody) (decompileRest rest))
        | P.Let(var, e1, e2) ->
            //todo: this needs to be handled better for curried functions
            applyParens OP.Let (sprintf "let%s%s = %s in %s" (if var.IsMutable then " mutable " else " ") var.Name (decompile CC.Zero e1) (decompile CC.Zero e2))
        | P.Quote(qx) ->
            //N.B. we have no way of differentiating betweened typed and untyped inner quotations; all come as untyped so that's the only kind we can support.
            sprintf "<@ %s @>" (decompile CC.Zero qx) 
        | DP.OrElse(DP.Bool(true), DP.Bool(false)) -> //true || false can't be distinguished from true && true, yet is less likely an expression due to short-circuiting
            applyParens OP.And "true && true"
        | DP.AndAlso(DP.Bool(false), DP.Bool(true)) -> //false && true can't be distinguished from false || false, yet is less likely an expression due to short-circuiting
            applyParens OP.Or "false || false"
        | DP.AndAlso(a,b) -> //must come before if then else
            applyParens OP.And (sprintf "%s && %s" (decompile (OP.And, OP.Left) a) (decompile (OP.And,OP.Right) b))
        | DP.OrElse(a,b) -> //must come before if then else
            applyParens OP.Or (sprintf "%s || %s" (decompile (OP.Or,OP.Left) a) (decompile (OP.Or,OP.Right) b))
        | P.IfThenElse(a,b, DP.Unit) -> //syntax doesn't require else branch when it's nothing but unit
            applyParens OP.If (sprintf "if %s then %s" (decompile (OP.If,OP.Non) a) (decompile (OP.If,OP.Non) b))
        | P.IfThenElse(a,b,c) ->
            applyParens OP.If (sprintf "if %s then %s else %s" (decompile (OP.If,OP.Non) a) (decompile (OP.If,OP.Non) b) (decompile (OP.If,OP.Non) c))
        //we can't reduce any XXXSet expressions due to limitations of Expr.Eval()
        | P.VarSet(v, arg) ->
            //not sure what precedence should be, using precedence for < op
            applyParens OP.LessThanOp (sprintf "%s <- %s" v.Name (decompile CC.Zero arg)) 
        //extremely verbose
        | P.UnionCaseTest(target, uci) ->
            let ucMatch =
                if uci |> ER.isListUnionCase then
                    if uci.Name = "Empty" then "[]"
                    else "_::_" //"Cons"
                else
                    let len = uci.GetFields().Length
                    if len = 0 then
                        sprintf "%s" uci.Name
                    else
                        sprintf "%s(%s)" uci.Name ("_" |> Array.create len |> String.concat ",")

            //using same precedence as if, 7, for match xxx with
            applyParens OP.If (sprintf "match %s with | %s -> true | _ -> false" (decompile (OP.If,OP.Non) target) ucMatch)
        | P.TryFinally(tryBody, finallyBody) ->
            applyParens OP.Try (sprintf "try %s finally %s" (decompile (OP.Try,OP.Non) tryBody) (decompile (OP.Try,OP.Non) finallyBody))
        | P.WhileLoop(condition,body) ->
            applyParens OP.While (sprintf "while %s do %s" (decompile (OP.While,OP.Non) condition) (decompile (OP.Try,OP.Non) body))
        | P.ForIntegerRangeLoop(var,rangeStart,rangeEnd,body) ->
            applyParens OP.For (sprintf "for %s in %s..%s do %s" var.Name (decompile CC.Zero rangeStart) (decompile CC.Zero rangeEnd) (decompile (OP.For,OP.Non) body))
        | P.TupleGet (x,n) ->
            sprintf "(%s)?Item%d" (decompile CC.Zero x) n            
        | _ -> 
            sprintf "%A" (expr)
    and decompileArgs prec delimiter exprs =
        exprs |> List.map (decompile prec) |> String.concat delimiter
    and decompileTupledArgs = 
        decompileArgs (OP.Comma,OP.Non) ", "
    and decompileCurriedArgs = //application of arguments to a function
        decompileArgs (OP.Application,OP.Non) " "
    and decompileSequencedArgs =
        decompileArgs (OP.Semicolon,OP.Non) "; "
    decompile CC.Zero expr

//-----operator precedence updated April 2011 with bitwise ops-----
//see OperatorPrecedenceedence.xlsx, not yet implemented.

//-----precedence-----
//note: http://stackoverflow.com/questions/4859151/help-me-understand-lambda-expression-precedence
//spec: http://research.microsoft.com/en-us/um/cambridge/projects/fsharp/manual/spec.html
//from spec:  Paren(token) pushed when (, begin, struct, sig, {, [, [|, or quote-op-left is encountered.
//custom operator precedence determined by first op in sequence: http://stackoverflow.com/questions/3347972/f-custom-operators-precedence
//precedence table: http://msdn.microsoft.com/en-us/library/dd233228.aspx
(*
        Operator        Associativity
    1   as              Right
    2   when            Right
    3   | (pipe)        Left
    4   ;               Right
    5   let             Nonassociative
    6   function , fun, 
        match, try      Nonassociative
    7   if              Nonassociative
    8   ->              Right
    9   :=              Right
    10  ,               Nonassociative
    11  or , ||         Left
    12  & , &&          Left
    13  < op, >op, =, 
        |op, &op        Left
    14  ^ op            Right
    15  ::              Right
    16  :?> , :?        Nonassociative
    17  - op, +op, 
        (binary)        Left
    18  * op, /op, %op  Left
    19  ** op           Right
    20  f x (function 
        application)    Left
    21  | (pattern 
        match)          Right
    22  prefix ops 
        (+op, -op, %, 
        %%, &, &&, 
        !op, ~op)       Left
    23  .               Left
    24  f(x)            Left
    25  f< types >      Left
*)


//operator lookup (from spec)
(*

[]    op_Nil

::    op_ColonColon

+     op_Addition

-     op_Subtraction

*     op_Multiply

/     op_Division

**    op_Exponentiation

@     op_Append       

^     op_Concatenate  

%     op_Modulus

&&&   op_BitwiseAnd

|||   op_BitwiseOr

^^^   op_ExclusiveOr

<<<   op_LeftShift

~~~   op_LogicalNot

>>>   op_RightShift

~+    op_UnaryPlus

~-    op_UnaryNegation

=     op_Equality

<>    op_Inequality

<=    op_LessThanOrEqual

>=    op_GreaterThanOrEqual

<     op_LessThan

>     op_GreaterThan

?     op_Dynamic

?<-   op_DynamicAssignment

|>    op_PipeRight

||>   op_PipeRight2

|||>  op_PipeRight3

<|    op_PipeLeft

<||   op_PipeLeft2

<|||  op_PipeLeft3

!     op_Dereference

>>    op_ComposeRight

<<    op_ComposeLeft

<@ @> op_Quotation

<@@ @@> op_QuotationUntyped

~%    op_Splice

~%%   op_SpliceUntyped

~&    op_AddressOf

~&&   op_IntegerAddressOf

||    op_BooleanOr

&&    op_BooleanAnd

+=    op_AdditionAssignment

-=    op_SubtractionAssignment

*=    op_MultiplyAssignment

/=    op_DivisionAssignment

..    op_Range

.. .. op_RangeStep

*)