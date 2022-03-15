

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
let mutable freshCounter = 0

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
    

// let rec graphPrintC e accStr i1 i2 = 
//     match e with
//     | AssignVarExpr(v,x) -> (accStr + "\n"+ (graphBuilder i1 (i1+1)) (v + ":=" + printA x) , i1+1)    //varDic.Add(v,evalAExpr(x))
//     | AssignArray(a,i,x) -> (accStr + "\n"+ (graphBuilder i1 (i1+1)) (a + "[" + printA(i) + "]" + ":=" + printA x), i1+1)
//     | Skip -> (accStr + "\n"+ graphBuilder i1 (i1+1) ("SKIP"), i1+1)
//     | CommandSeq(c1,c2) -> let (accNew,xNew) = graphPrintC c1 accStr i1
//                            graphPrintC c2 accNew xNew
//     | IfExpr(g) -> ("",System.Byte.MaxValue)
//     | DoExpr(g) -> ("",0)
    
// We
let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = AssignmentParser.start AssignmentLexer.tokenize lexbuf
    // return the result of parsing (i.e. value of type "expr")
    res
(*
let rec compileC inputC firstState secondState noOfNewStates =
        match inputC with
        | AssignVarExpr(v,x) -> sprintf "%s -> %s [label = \"%s:=%s\"];\n" firstState secondState v (printA x)
        // | IfCommandExpr(s) -> String.Format("[label = a \"{0}\"]", s)
        | CommandSeq(c1,c2) -> (compileC c1 firstState (sprintf "q%d" (noOfNewStates+1)) (noOfNewStates+1)) + (compileC c2 (sprintf "q%d" (noOfNewStates+1)) secondState (noOfNewStates+1))
*)      

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




