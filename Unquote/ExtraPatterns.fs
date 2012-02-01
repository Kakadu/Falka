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

///Extra Quoation patterns for sprinting and reducing Quotation Expressions
module internal Swensen.Unquote.ExtraPatterns
open System
open System.Reflection
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations

module P = Microsoft.FSharp.Quotations.Patterns
module DP = Microsoft.FSharp.Quotations.DerivedPatterns
module OP = Swensen.Unquote.OperatorPrecedence
//module ER = Swensen.Unquote.ExtraReflection

open Swensen.Utils

let binaryOps = 
    [
    //boolean ops
    "op_Equality", ("=", OP.EqualsOp)
    "op_GreaterThan", (">", OP.GreaterThanOp)
    "op_LessThan", ("<", OP.LessThanOp)
    "op_GreaterThanOrEqual", (">=", OP.GreaterThanOp)
    "op_LessThanOrEqual", ("<=", OP.LessThanOp)
    "op_Inequality", ("<>", OP.LessThanOp)
    //pipe ops
    "op_PipeRight", ("|>", OP.Pipe) //might need to be OP.PipeOp
    "op_PipeRight2", ("||>", OP.Pipe)
    "op_PipeRight3", ("|||>", OP.Pipe)
    "op_PipeLeft", ("<|", OP.LessThanOp)
    "op_PipeLeft2", ("<||", OP.LessThanOp)
    "op_PipeLeft3", ("<|||", OP.LessThanOp)
    //numeric ops
    "op_Addition", ("+", OP.PlusBinaryOp)
    "op_Subtraction", ("-", OP.MinusBinaryOp)
    "op_Division", ("/", OP.DivideOp)
    "op_Multiply", ("*", OP.MultiplyOp)
    "op_Modulus", ("%", OP.ModOp)
    "op_Exponentiation", ("**", OP.ExponentiationOp)
    //bit operators
    "op_BitwiseAnd", ("&&&", OP.BitwiseAnd)
    "op_BitwiseOr", ("|||", OP.BitwiseOr)
    "op_ExclusiveOr", ("^^^", OP.ExclusiveOr)
    "op_LeftShift", ("<<<", OP.LeftShift)
    "op_RightShift", (">>>", OP.RightShift)

    //composition
    "op_ComposeRight", (">>", OP.GreaterThanOp)
    "op_ComposeLeft", ("<<", OP.LessThanOp)
    //special
    "op_Append", ("@", OP.AppendOp) //not sure what precedence, falling back on (+)
    "op_Concatenate", ("^", OP.ConcatenateOp) //ocaml style string concatentation
    //set ref cell
    "op_ColonEquals", (":=", OP.RefAssign)
    ] |> Map.ofList

//future feature, support custom ops
///Match non-custom binary infix Call patterns.
///Must come before Call pattern.
let (|BinaryInfixCall|_|) = function
    | P.Call (_, mi, lhs::rhs::[]) ->
        match binaryOps |> Map.tryFind mi.Name with
        | Some op -> Some(op,lhs,rhs)
        | None -> None
    | _ -> None

let unaryOps = 
    [
    "op_UnaryPlus", "+"
    "op_UnaryNegation", "-"
    "op_LogicalNot", "~~~"
    "op_Dereference", "!"
    ] |> Map.ofList

//all unary ops have precedence of 9
let (|UnaryPrefixCall|_|) = function
    | P.Call (_, mi, arg::[]) ->
        match unaryOps |> Map.tryFind mi.Name with
        | Some(op) -> Some(op, arg)
        | None -> None
    | _ -> None

//suprisingly, this is actually used twice.
///Test whether the Expr is a Var and equals the given Var property-wise
let private isVarOfExpr (x:Var) = function
    | P.Var y | P.Coerce(P.Var y,_) -> x.Name = y.Name && x.Type = y.Type && x.IsMutable = y.IsMutable
    | _ -> false

///Test whether the given expression represents a tuple let binding: e.g. let x,y = 1,2.
///Must come before Let pattern and after IncompleteLambdaCall pattern.
let (|TupleLet|_|) x =
    //N.B. breaking out the two TupleLetStart variations allows us to using | pattern match with start and body binding.

    ///TupleLet start variation 1) let a = TupleGet(tupleProperty, index) in let b = TupleGet(tupleProperty, index) in ...
    let (|PropertyTuple|_|) = function
        | (P.Let(_,P.TupleGet(propertyTuple, _),_) as bindings) ->
            Some(bindings, propertyTuple)
        | _ -> None

    ///TupleLet start variation 2) let patternInput = expression in let a = TupleGet(patternInput, index) in ...
    let (|PatternInputTuple|_|) = function
        //this is getting a little crazy, but it is the observed pattern, and pi = piAgain is a necessary restriction
        //so as to not have too wide a net.
        | P.Let(var, patternInputTuple, (P.Let(_,P.TupleGet(varExpr,_),_) as bindings)) when isVarOfExpr var varExpr ->
            Some(bindings, patternInputTuple)
        | _ -> None

    match x with
    | PropertyTuple(bindings,tuple) | PatternInputTuple(bindings,tuple) ->
        //e.g., the "var index list" for let (a,_,b,_,_,c,_,_,_) would be [(a,0);(b,2);(c,5)]
        //order can either be forward (lambda expressions) or in reverse (normal tuple let bindings)
        let tupledVars = Array.create (FSharpType.GetTupleElements(tuple.Type).Length) (None:Var option)
        let rec fillVarsAndGetBody = function
            | P.Let(var,P.TupleGet(_,index),next) ->
                tupledVars.[index] <- Some(var)
                fillVarsAndGetBody next
            | final -> final
        
        let body = fillVarsAndGetBody bindings

        Some(tupledVars |> Array.toList, tuple, body)
    | _ -> None

////need to check all args are reduced?
///Partial application and zero application of Lambda call (e.g. List.map (+), or id).
///Must come before Let and Lambdas patterns.
///Cases: 1) Let .. Lambdas .. Call
///       2) Lambdas .. Call
let (|IncompleteLambdaCall|_|) x =
    match x with
    | (P.Let _ | P.Lambda _) -> //this is definately not a complete lambda call
        let rec gatherLetBindings varsList bindingList = function
            | TupleLet(vars, binding, body) -> 
                gatherLetBindings ((vars |> List.choose id)::varsList) (binding::bindingList) body
            | P.Let(var, binding, body) -> 
                gatherLetBindings ([var]::varsList) (binding::bindingList) body
            | final -> 
                varsList |> List.rev, bindingList |> List.rev, final

        let varsList, bindingList, final = gatherLetBindings [] [] x

        match final with
        | DP.Lambdas(lambdaVarsList, P.Call(target, mi, callArgs)) 
            //requiring all callArgs to be Vars is a temp cheat till we know how to deal with properties in call args **    
            when List.equalsWith isVarOfExpr ((varsList |> List.concat) @ (lambdaVarsList |> List.concat)) callArgs -> 
                Some(target, mi, bindingList)
        | _ -> None
    | _ -> None

//only used by Range and RangeStep
let private rangeOuterInnerMethodInfos (miOuter:MethodInfo) (miInner:MethodInfo) conversionMiName =
    miInner.DeclaringType.FullName = "Microsoft.FSharp.Core.Operators" && miInner.Name = "CreateSequence" 
        && miOuter.DeclaringType.FullName = "Microsoft.FSharp.Collections.SeqModule" && miOuter.Name = conversionMiName

///Match a sequence, list, or array op_Range expression, return (startToken, endToken, startExpression, endExpression). Must come before Call patterns.
let (|Range|_|) x =
    let (|RangeOp|_|) = function
        | P.Call(None, mi, a::b::_) when mi.Name = "op_Range" -> Some(a,b)
        | _ -> None
    
    match x with 
    | RangeOp(a,b) -> 
        Some("{","}",a,b)
    | P.Call(None, miOuter, [P.Call(None, miInner, [RangeOp(a,b)])]) when rangeOuterInnerMethodInfos miOuter miInner "ToList" -> 
        Some("[","]",a,b)
    | P.Call(None, miOuter, [P.Call(None, miInner, [RangeOp(a,b)])]) when rangeOuterInnerMethodInfos miOuter miInner "ToArray" -> 
        Some("[|", "|]", a,b)
    | _ -> None

///Match a sequence, list, or array op_RangeStep expression, return (startToken, endToken, startExpression, stepExpression, endExpression). Must come before Call patterns.
let (|RangeStep|_|) x =
    let (|RangeStepOp|_|) = function
        | P.Call(None, mi, a::b::c::_) when mi.Name = "op_RangeStep" -> Some(a,b,c)
        | _ -> None

    match x with
    | RangeStepOp(a,b,c) -> 
        Some("{","}",a,b,c)
    | P.Call(None, miOuter, [P.Call(None, miInner, [RangeStepOp(a,b,c)])]) when rangeOuterInnerMethodInfos miOuter miInner "ToList" -> 
        Some("[","]",a,b,c)
    | P.Call(None, miOuter, [P.Call(None, miInner, [RangeStepOp(a,b,c)])]) when rangeOuterInnerMethodInfos miOuter miInner "ToArray" -> 
        Some("[|", "|]", a,b,c)
    | _ -> None

//let (|FunctionCall|_|) = function
//    | Call(target, mi, args) when ER.isFunction mi -> 