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
module internal Swensen.Unquote.OperatorPrecedence

type assoc =
| Non
| Left
| Right

type OperatorPrecedence(precedence:int, ?associativity:assoc) = 
    let associativity = match associativity with Some(a) -> a | None -> Non
    ///Precedence
    member __.Precedence = precedence
    ///Associativity
    member __.Associativity = associativity

type OP = OperatorPrecedence

//"Op" suffix indicates a legitimate customizable op
let As = OP(1,Right)
let When = OP(2,Right)
let Pipe = OP(3,Left)
let Semicolon = OP(4,Right) //Sequential
let Let = OP(5)
let Function,Fun,Match,Try,While,For = let p() = OP(6) in p(),p(),p(),p(),p(),p() //While and For are not in spec, but seems at home here
let If = OP(7)
let RightArrow = OP(8,Right)
let RefAssign = OP(9,Right)
let Comma = OP(10)
let Or = OP(11,Left) //note "or" is deprecated form of "||"
let And = OP(12,Left) //note "&" is deprecated form of "&&"
let LessThanOp,GreaterThanOp,EqualsOp,PipeOp,AndOp = let p() = OP(13,Left) in p(),p(),p(),p(),p()
let BitwiseAnd,BitwiseOr,ExclusiveOr,LogicalNot,LeftShift,RightShift = let p() = OP(14,Left) in p(),p(),p(),p(),p(),p()
let ConcatenateOp = OP(15,Right) //OCaml string concat
let Cons = OP(16,Right)
let AppendOp = OP(17,Left) //not sure, empirical
let DynamicCast,TypeTest = let p() = OP(17) in p(),p()
let MinusBinaryOp,PlusBinaryOp = let p() = OP(18,Left) in p(),p()
let MultiplyOp,DivideOp,ModOp = let p() = OP(19,Left) in p(),p(),p()
let ExponentiationOp = OP(20,Right)
let Application = OP(21,Left)
let PatternMatch = OP(22,Right)
let PrefixOps = OP(23,Left)
let Dot = OP(24,Left)
let MethodCall = OP(25,Left)
let TypeArguments = OP(26,Left)

let applyParensForPrecInContext (contextOP:OperatorPrecedence) contextAssoc (localOP:OperatorPrecedence) s = 
    if contextOP = Application && localOP = MethodCall then //special rule
        sprintf "(%s)" s
    else //normal rules
        let context =
            match contextOP.Associativity, contextAssoc with
            | Left, Left | Right, Right -> contextOP.Precedence - 1
            | _ -> contextOP.Precedence
    
        if localOP.Precedence > context then s else sprintf "(%s)" s