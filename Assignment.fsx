// MADE BY: Kevin Moore s204462, Aryan Mirzazadeh s204489, Jakob Jacobsen s204502, Bjørn Laursen s204451

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

// MAX INTEGER
type numVar = int

//let mutable initVarMap:(Map<string, float>) = Map.ofList []
//let mutable initArrMap:(Map<string, Array>) = Map.ofList []

///////////////////////////////
/// TASK 1: PRETTY PRINTING ///
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
///////////////////////////////
//////////// TASK 2 ///////////
///////////////////////////////

// q▷ -> q1 [label = "y:=1"];
// q1 -> q2 [label = "x>0"];
// q2 -> q3 [label = "y:=x*y"];
// q3 -> q1 [label = "x:=x-1"];
// q1 -> q◀ [label = "!(x>0)"];

// AUX functions for printing
(*let mutable freshCounter = 0

let printNode i = 
    if i = 0 then "q\u25B7"
    else if i = numVar.MaxValue then "q\u25C0"
    else (sprintf "q%i" i)

let graphBuilder int1 int2 labelString = printNode int1 + " -> " + printNode int2 + " [label = \"" + labelString + "\"];"


let rec printA e : string =
    match e with
    | Num(x) -> (string x)
    | Var(x) -> x
    | ListAExpr(x,y) -> x + "["+ (printA y)+"]"
    | TimesExpr(x,y) -> "(" + (printA x) + "*" + (printA y) + ")"
    | DivExpr(x,y) -> "(" + (printA x) + "/" + (printA y) + ")"
    | PlusExpr(x,y) -> "(" + (printA x) + "+" + (printA y) + ")"
    | MinusExpr(x,y) -> "(" + (printA x) + "-" + (printA y) + ")"
    | PowExpr(x,y) -> "(" + printA(x) + "^" + printA(y) + ")"
    | UPlusExpr(x) -> printA(x)
    | UMinusExpr(x) -> "-(" + printA(x) + ")" 

let rec printB b : string =
    match b with
    | BoolExpr(b) -> if b then "true" else "false"
    | AndExpr(b1,b2) -> "("+printB(b1) + ")&(" + printB(b2) + ")"
    | OrExpr(b1,b2) -> "("+printB(b1) + ")|(" + printB(b2) + ")"
    | SCAndExpr(b1,b2) -> "("+printB(b1) + ")&&(" + printB(b2) + ")"
    | SCOrExpr(b1,b2) -> "("+printB(b1) + ")||(" + printB(b2) + ")"
    | NotExpr(b) -> "!("+printB(b)+")"
    | EqExpr(a1,a2) -> printA(a1) + "=" + printA(a2)
    | NotEqExpr(a1,a2) -> printA(a1) + "!=" + printA(a2)
    | GrExpr(a1,a2) -> printA(a1) + ">" + printA(a2)
    | GrEqExpr(a1,a2) -> printA(a1) + ">=" + printA(a2)
    | LeExpr(a1,a2) -> printA(a1) + "<" + printA(a2)
    | LeEqExpr(a1,a2) -> printA(a1) + "<=" + printA(a2)


let rec graphPrintC e accStr (i1:int) (i2:int) : (string) = 
    match e with
    | AssignVarExpr(v,x) -> accStr + "\n"+ (graphBuilder i1 i2) (v + ":=" + printA x)
    | AssignArray(a,i,x) -> accStr + "\n"+ (graphBuilder i1 i2) (a + "[" + printA(i) + "]" + ":=" + printA x)
    | Skip -> accStr + "\n"+ graphBuilder i1 i2 "SKIP"
    | CommandSeq(c1,c2) -> freshCounter <- freshCounter + 1
                           let count = freshCounter
                           let strNew = (graphPrintC c1 accStr i1 count)
                           graphPrintC c2 strNew count i2
    | IfExpr(g) -> graphPrintGC g accStr i1 i2
    | DoExpr(g) -> let strNew = graphPrintGC g accStr i1 i1
                   doneGraphPrintGC g strNew i1 i2

and graphPrintGC g accStr i1 i2 =
    match (g) with
    | BoolGC(b,c) -> freshCounter <- freshCounter + 1
                     graphPrintC c (accStr + "\n" + (graphBuilder i1 freshCounter (printB b))) freshCounter i2
    | GCSequence(g1,g2) -> graphPrintGC g2 (graphPrintGC g1 accStr i1 i2) i1 i2

and doneGraphPrintGC g accStr i1 i2 =
    match (g) with
    | BoolGC(b,c) -> freshCounter <- freshCounter + 1
                     accStr + "\n" + graphBuilder i1 i2 (printB (NotExpr(b)))
    | GCSequence(g1,g2) -> doneGraphPrintGC g2 (doneGraphPrintGC g1 accStr i1 i2) i1 i2

// We parse the input 
let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = AssignmentParser.start AssignmentLexer.tokenize lexbuf
    // return the result of parsing (i.e. value of type "expr")
    res

// We implement here the function that interacts with the user
let rec compute n =
    freshCounter <- 0
    printf "Enter an expression: "
    try
        // We parse the input string
        let e = parse (Console.ReadLine())

        // and print the result of evaluating it
        let initialCode = "digraph program_graph {rankdir=LR;\nnode [shape = circle]; q\u25B7;\nnode [shape = doublecircle]; q\u25C0;\nnode [shape = circle]\n"

        printfn "Result: \n%s%s" (graphPrintC e initialCode 0 numVar.MaxValue) "}"
    with err -> compute n


*)
/// TASK 3 ///

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

let rec evalBExpr b =
    match b with
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
    | BoolExpr(b) -> b

let rec evalCommand e =
    match e with
    | AssignVarExpr(v,x) -> let ok = varDic.Remove(v)
                            varDic.Add(v,evalAExpr(x))
                            None
    | AssignArray(a,i,x) -> None
    | Skip -> None
    | CommandSeq(c1,c2) -> let ok = evalCommand c1
                           evalCommand c2
    | IfExpr(g) -> evalGuardedCommand g
    | DoExpr(g) -> None
and evalGuardedCommand e =
    match e with
    | BoolGC(b,c) -> if (evalBExpr b) then (evalCommand c) else evalCommand Skip
    | GCSequence(g1,g2) ->  let ok = evalGuardedCommand g1
                            evalGuardedCommand g2


    
// We

let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = AssignmentParser.start AssignmentLexer.tokenize lexbuf
    // return the result of parsing (i.e. value of type "expr")
    res

let rec compute n =
    printf "Enter an expression: "
    try
        // We parse the input string
        let e = parse (Console.ReadLine())
        let ok = evalCommand e

        // and print the result of evaluating it

        printfn "Result:"
        for pair in varDic do
            printfn "%A" pair
    with err -> compute n

// Start interacting with the user
// compute 3
