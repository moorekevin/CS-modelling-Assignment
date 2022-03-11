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
// x:=0
(*
let rec printC c =
    match c with
    | AssignVarExpr(v,x) -> "ASSIGN ("+ v + ", " + printA(x) + ")"    //varDic.Add(v,evalAExpr(x))
    | AssignArray(a,i,x) -> "ASSIGN ("+ a + "[" + printA(i) + "] , " + printA(x) + ")" 
    | Skip -> "SKIP "
    | CommandSeq(c1,c2) -> "COMMANDSeq (" + printC(c1) + " ; " + printC(c2) + ")"
    | IfExpr(g) -> "IF (" + printGC(g) + ")"
    | DoExpr(g) -> "DO (" + printGC(g) + ")"
and printGC g =
    match g with
    | BoolGC(b,c) -> "BoolGC(" + printB(b) + " -> " + printC(c) + ")"  // bool -> GC
    | GCSequence(g1,g2) -> "GCSeq (" + printGC(g1) + " [] " + printGC(g2) + ")"
and printB b =
    match b with
    | BoolExpr(b) -> if b then "true" else "false"
    | AndExpr(b1,b2) -> "("+printB(b1) + "&" + printB(b2)+")"
    | OrExpr(b1,b2) -> "("+printB(b1) + "|" + printB(b2)+")"
    | SCAndExpr(b1,b2) -> "("+printB(b1) + "&&" + printB(b2)+")"
    | SCOrExpr(b1,b2) -> "("+printB(b1) + "||" + printB(b2)+")"
    | NotExpr(b) -> "!("+printB(b)+")"
    | EqExpr(a1,a2) -> "(" + printA(a1) + "==" + printA(a2) + ")"
    | NotEqExpr(a1,a2) -> "(" + printA(a1) + "!=" + printA(a2) + ")"
    | GrExpr(a1,a2) -> "(" + printA(a1) + ">" + printA(a2) + ")"
    | GrEqExpr(a1,a2) -> "(" + printA(a1) + ">=" + printA(a2) + ")"
    | LeExpr(a1,a2) -> "(" + printA(a1) + "<" + printA(a2) + ")"
    | LeEqExpr(a1,a2) -> "(" + printA(a1) + "<=" + printA(a2) + ")"
and printA e : string =
    match e with
    | Num(x) -> (string x) //string x
    | Var(x) -> x
    | ListAExpr(x,y) -> x + "["+ (printA y)+"]"
    | TimesExpr(x,y) -> "TIMES (" + (printA x) + ", " + (printA y) + ")"
    | DivExpr(x,y) -> "DIV (" + (printA x) + ", " + (printA y) + ")"
    | PlusExpr(x,y) -> "PLUS (" + (printA x) + ", " + (printA y) + ")"
    | MinusExpr(x,y) -> "MINUS (" + (printA x) + ", " + (printA y) + ")"
    | PowExpr(x,y) -> "POWER (" + printA(x) + ", " + printA(y) + ")"
    | UPlusExpr(x) -> "+" + printA(x)
    | UMinusExpr(x) -> "-(" + printA(x) + ")" 
*)

// We
let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = AssignmentParser.start AssignmentLexer.tokenize lexbuf
    // return the result of parsing (i.e. value of type "expr")
    res

// We implement here the function that interacts with the user
let rec compute n =
    printf "Enter an expression: "
    try
        // We parse the input string
        let e = parse (Console.ReadLine())

        // and print the result of evaluating it
        printfn "Result: %s" (printC e)
        compute n-1
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