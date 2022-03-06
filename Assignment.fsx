// This script implements our interactive calculator

// We need to import a couple of modules, including the generated lexer and parser
#r "/Users/kevinmoore/fsharp/FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"
open FSharp.Text.Lexing
open System

#load "/Users/kevinmoore/Documents/DTU/Datalogisk Modellering/Assignment.nosync/CS-modelling-Assignment/AssignmentTypesAST.fs"
open AssignmentTypesAST

#load "/Users/kevinmoore/Documents/DTU/Datalogisk Modellering/Assignment.nosync/CS-modelling-Assignment/AssignmentParser.fs"
open AssignmentParser

#load "/Users/kevinmoore/Documents/DTU/Datalogisk Modellering/Assignment.nosync/CS-modelling-Assignment/AssignmentLexer.fs"
open AssignmentLexer

// We define the evaluation function recursively, by induction on the structure
// of arithmetic expressions (AST of type expr)
let rec evalAExpr e =
  match e with
    | Num(x) -> x
    | TimesExpr(x,y) -> eval(x) * eval (y)
    | DivExpr(x,y) -> eval(x) / eval (y)
    | PlusExpr(x,y) -> eval(x) + eval (y)
    | MinusExpr(x,y) -> eval(x) - eval (y)
    | PowExpr(x,y) -> eval(x) ** eval (y)
    | UPlusExpr(x) -> eval(x)
    | UMinusExpr(x) -> - eval(x)
    | Var(x) -> 
    | ListAExpr(x,y) -> 
and evalCommand e =
    match e with
    | AssignVarExpr(x,v) -> 
    | AssignArray(a,i,v) -> 
    | Skip -> 
    | CommandSeq(c1,c2) -> evalCommand c1
                           evalCommand c2
    | IfExpr((b,c) :: res) -> if evalBExpr(b) then evalCommand(c) else Skip
    | DoExpr(g) -> 
and evalGuardedCommand e =
    match e with
    | BoolGC(b,c) -> [(b,c)]
    | GCSequence(g1,g2) -> (evalGuardedCommand g1) @ (evalGuardedCommand g2)


let rec evalBExpr b =
    match b with
    | True -> true
    | False -> false
    | AndExpr(b1,b2) -> evalBExpr(b1) & evalBExpr(b2)
    | OrExpr(b1,b2) -> let b1 = evalBExpr(b1)
                       let b2 = evalBExpr(b2)
                       b1 || b2
    | SCAndExpr(b1,b2) -> evalBExpr(b1) && evalBExpr(b2)
    | SCOrExpr(b1,b2) -> evalBExpr(b1) || evalBExpr(b2)
    | NotExpr(b) -> not evalBExpr(b)
    | EqExpr(a1,a2) -> evalAExpr(a1) == evalAExpr(a2)
    | NotEqExpr(a1,a2) -> evalAExpr(a1) <> evalAExpr(a2)
    | GrExpr(a1,a2) -> evalAExpr(a1) > evalAExpr(a2)
    | GrEqExpr(a1,a2) -> evalAExpr(a1) >= a2
    | LeExpr(a1,a2) -> evalAExpr(a1) < evalAExpr(a2)
    | LeEqExpr(a1,a2) -> evalAExpr(a1) <= evalAExpr(a2)


// if
// true -> x = x+1
// false -> x:=2
// fi

// We
// let parse input =
//     // translate string into a buffer of characters
//     let lexbuf = LexBuffer<char>.FromString input
//     // translate the buffer into a stream of tokens and parse them
//     let res = AssignmentParser.start AssignmentLexer.tokenize lexbuf
//     // return the result of parsing (i.e. value of type "expr")
//     res

// We implement here the function that interacts with the user
// let rec compute n =
//     if n = 0 then
//         printfn "Bye bye"
//     else
//         printf "Enter an arithmetic expression: "
//         try
//         // We parse the input string
//         let e = parse (Console.ReadLine())
//         // and print the result of evaluating it
//         printfn "Result: %f" (eval(e))
//         compute n
//         with err -> compute (n-1)

// Start interacting with the user
// compute 3