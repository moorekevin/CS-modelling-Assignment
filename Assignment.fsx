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


//let mutable initVarMap:(Map<string, float>) = Map.ofList []
//let mutable initArrMap:(Map<string, Array>) = Map.ofList []

open System.Collections.Generic
let varDic = new Dictionary<string,float>()
let arrDic = new Dictionary<string,Array>()

// We define the evaluation function recursively, by induction on the structure
// of arithmetic expressions (AST of type expr)
let rec evalAExpr e =
  match e with
    | Num(x) -> x
    | TimesExpr(x,y) -> evalAExpr(x) * evalAExpr (y)
    | DivExpr(x,y) -> evalAExpr(x) / evalAExpr (y)
    | PlusExpr(x,y) -> evalAExpr(x) + evalAExpr (y)
    | MinusExpr(x,y) -> evalAExpr(x) - evalAExpr (y)
    | PowExpr(x,y) -> evalAExpr(x) ** evalAExpr (y)
    | UPlusExpr(x) -> evalAExpr(x)
    | UMinusExpr(x) -> - evalAExpr(x)
    | Var(x) -> varDic.[x]
    | ListAExpr(x,y) -> 1.0

let rec evalCommand e =
    match e with
    | AssignVarExpr(v,x) -> varDic.Add(v,evalAExpr(x))
    | AssignArray(a,i,x) -> printf("TODO")
    | Skip -> printf("TODO")
    | CommandSeq(c1,c2) -> evalCommand c1
                           evalCommand c2
    | IfExpr(g) -> printf("TODO") //if evalBExpr(b) then evalCommand(c) else Skip
    | DoExpr(g) -> printf("TODO")
and evalGuardedCommand e =
    match e with
    | BoolGC(b,c) -> [(b,c)]
    | GCSequence(g1,g2) -> (evalGuardedCommand g1) @ (evalGuardedCommand g2)

let rec evalBExpr b =
    match b with
    | True -> true
    | False -> false
    | AndExpr(b1,b2) -> let b1 = evalBExpr(b1)
                        let b2 = evalBExpr(b2)
                        b1 && b2
    | OrExpr(b1,b2) -> let b1 = evalBExpr(b1)
                       let b2 = evalBExpr(b2)
                       b1 || b2
    | SCAndExpr(b1,b2) -> evalBExpr(b1) && evalBExpr(b2)
    | SCOrExpr(b1,b2) -> evalBExpr(b1) || evalBExpr(b2)
    | NotExpr(b) -> not (evalBExpr(b))
    | EqExpr(a1,a2) -> (evalAExpr(a1)) = (evalAExpr(a2))
    | NotEqExpr(a1,a2) -> evalAExpr(a1) <> evalAExpr(a2)
    | GrExpr(a1,a2) -> evalAExpr(a1) > evalAExpr(a2)
    | GrEqExpr(a1,a2) -> evalAExpr(a1) >= evalAExpr(a2)
    | LeExpr(a1,a2) -> evalAExpr(a1) < evalAExpr(a2)
    | LeEqExpr(a1,a2) -> evalAExpr(a1) <= evalAExpr(a2)

// We
let parse input =
    printf "1"
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    printf "2"
    // translate the buffer into a stream of tokens and parse them
    let res = AssignmentParser.start AssignmentLexer.tokenize lexbuf
    printf "3"
    // return the result of parsing (i.e. value of type "expr")
    res

// We implement here the function that interacts with the user
let rec compute n =
    printf "Enter an expression: "
    try
        // We parse the input string
        let e = parse (Console.ReadLine())

        // and print the result of evaluating it
        printfn "Result: " 
        compute n
    with err -> compute n

// -> (Map<string,float>, Map<string,Array>)

// Assign var:
// x := 1
// Assign array:
// x[1] := 1
// get var:
// x
// get array:
// x[1]

// Start interacting with the user
// compute 3