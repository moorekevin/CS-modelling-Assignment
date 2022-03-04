// Implementation file for parser generated by fsyacc
module CalculatorParser
#nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
open FSharp.Text.Lexing
open FSharp.Text.Parsing.ParseHelpers
# 2 "/Users/kevinmoore/Documents/DTU/Datalogisk Modellering/FSharp/mandatory-assignment-master-303b1e1a5ced82e14a9bce5662a3be036aaf73d7/calculator/CalculatorParser.fsp"

open CalculatorTypesAST

# 10 "/Users/kevinmoore/Documents/DTU/Datalogisk Modellering/FSharp/mandatory-assignment-master-303b1e1a5ced82e14a9bce5662a3be036aaf73d7/calculator/CalculatorParser.fs"
// This type is the type of tokens accepted by the parser
type token = 
  | TIMES
  | DIV
  | PLUS
  | MINUS
  | POW
  | LPAR
  | RPAR
  | EOF
  | NUM of (float)
// This type is used to give symbolic names to token indexes, useful for error messages
type tokenId = 
    | TOKEN_TIMES
    | TOKEN_DIV
    | TOKEN_PLUS
    | TOKEN_MINUS
    | TOKEN_POW
    | TOKEN_LPAR
    | TOKEN_RPAR
    | TOKEN_EOF
    | TOKEN_NUM
    | TOKEN_end_of_input
    | TOKEN_error
// This type is used to give symbolic names to token indexes, useful for error messages
type nonTerminalId = 
    | NONTERM__startstart
    | NONTERM_start
    | NONTERM_expression0
    | NONTERM_expression1
    | NONTERM_expression2
    | NONTERM_expression3

// This function maps tokens to integer indexes
let tagOfToken (t:token) = 
  match t with
  | TIMES  -> 0 
  | DIV  -> 1 
  | PLUS  -> 2 
  | MINUS  -> 3 
  | POW  -> 4 
  | LPAR  -> 5 
  | RPAR  -> 6 
  | EOF  -> 7 
  | NUM _ -> 8 

// This function maps integer indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_TIMES 
  | 1 -> TOKEN_DIV 
  | 2 -> TOKEN_PLUS 
  | 3 -> TOKEN_MINUS 
  | 4 -> TOKEN_POW 
  | 5 -> TOKEN_LPAR 
  | 6 -> TOKEN_RPAR 
  | 7 -> TOKEN_EOF 
  | 8 -> TOKEN_NUM 
  | 11 -> TOKEN_end_of_input
  | 9 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__startstart 
    | 1 -> NONTERM_start 
    | 2 -> NONTERM_expression0 
    | 3 -> NONTERM_expression0 
    | 4 -> NONTERM_expression0 
    | 5 -> NONTERM_expression1 
    | 6 -> NONTERM_expression1 
    | 7 -> NONTERM_expression1 
    | 8 -> NONTERM_expression2 
    | 9 -> NONTERM_expression2 
    | 10 -> NONTERM_expression3 
    | 11 -> NONTERM_expression3 
    | 12 -> NONTERM_expression3 
    | 13 -> NONTERM_expression3 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 11 
let _fsyacc_tagOfErrorTerminal = 9

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | TIMES  -> "TIMES" 
  | DIV  -> "DIV" 
  | PLUS  -> "PLUS" 
  | MINUS  -> "MINUS" 
  | POW  -> "POW" 
  | LPAR  -> "LPAR" 
  | RPAR  -> "RPAR" 
  | EOF  -> "EOF" 
  | NUM _ -> "NUM" 

// This function gets the data carried by a token as an object
let _fsyacc_dataOfToken (t:token) = 
  match t with 
  | TIMES  -> (null : System.Object) 
  | DIV  -> (null : System.Object) 
  | PLUS  -> (null : System.Object) 
  | MINUS  -> (null : System.Object) 
  | POW  -> (null : System.Object) 
  | LPAR  -> (null : System.Object) 
  | RPAR  -> (null : System.Object) 
  | EOF  -> (null : System.Object) 
  | NUM _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
let _fsyacc_gotos = [| 0us; 65535us; 1us; 65535us; 0us; 1us; 4us; 65535us; 0us; 2us; 7us; 4us; 8us; 5us; 23us; 6us; 6us; 65535us; 0us; 9us; 7us; 9us; 8us; 9us; 12us; 10us; 13us; 11us; 23us; 9us; 7us; 65535us; 0us; 14us; 7us; 14us; 8us; 14us; 12us; 14us; 13us; 14us; 16us; 15us; 23us; 14us; 9us; 65535us; 0us; 17us; 7us; 17us; 8us; 17us; 12us; 17us; 13us; 17us; 16us; 17us; 18us; 19us; 20us; 21us; 23us; 17us; |]
let _fsyacc_sparseGotoTableRowOffsets = [|0us; 1us; 3us; 8us; 15us; 23us; |]
let _fsyacc_stateToProdIdxsTableElements = [| 1us; 0us; 1us; 0us; 3us; 1us; 2us; 3us; 1us; 1us; 3us; 2us; 2us; 3us; 3us; 2us; 3us; 3us; 3us; 2us; 3us; 13us; 1us; 2us; 1us; 3us; 3us; 4us; 5us; 6us; 3us; 5us; 5us; 6us; 3us; 5us; 6us; 6us; 1us; 5us; 1us; 6us; 2us; 7us; 8us; 2us; 8us; 8us; 1us; 8us; 1us; 9us; 1us; 10us; 1us; 10us; 1us; 11us; 1us; 11us; 1us; 12us; 1us; 13us; 1us; 13us; |]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us; 2us; 4us; 8us; 10us; 14us; 18us; 22us; 24us; 26us; 30us; 34us; 38us; 40us; 42us; 45us; 48us; 50us; 52us; 54us; 56us; 58us; 60us; 62us; 64us; |]
let _fsyacc_action_rows = 25
let _fsyacc_actionTableElements = [|4us; 32768us; 2us; 18us; 3us; 20us; 5us; 23us; 8us; 22us; 0us; 49152us; 3us; 32768us; 2us; 7us; 3us; 8us; 7us; 3us; 0us; 16385us; 2us; 16386us; 2us; 7us; 3us; 8us; 2us; 16387us; 2us; 7us; 3us; 8us; 3us; 32768us; 2us; 7us; 3us; 8us; 6us; 24us; 4us; 32768us; 2us; 18us; 3us; 20us; 5us; 23us; 8us; 22us; 4us; 32768us; 2us; 18us; 3us; 20us; 5us; 23us; 8us; 22us; 2us; 16388us; 0us; 12us; 1us; 13us; 2us; 16389us; 0us; 12us; 1us; 13us; 2us; 16390us; 0us; 12us; 1us; 13us; 4us; 32768us; 2us; 18us; 3us; 20us; 5us; 23us; 8us; 22us; 4us; 32768us; 2us; 18us; 3us; 20us; 5us; 23us; 8us; 22us; 1us; 16391us; 4us; 16us; 1us; 16392us; 4us; 16us; 4us; 32768us; 2us; 18us; 3us; 20us; 5us; 23us; 8us; 22us; 0us; 16393us; 4us; 32768us; 2us; 18us; 3us; 20us; 5us; 23us; 8us; 22us; 0us; 16394us; 4us; 32768us; 2us; 18us; 3us; 20us; 5us; 23us; 8us; 22us; 0us; 16395us; 0us; 16396us; 4us; 32768us; 2us; 18us; 3us; 20us; 5us; 23us; 8us; 22us; 0us; 16397us; |]
let _fsyacc_actionTableRowOffsets = [|0us; 5us; 6us; 10us; 11us; 14us; 17us; 21us; 26us; 31us; 34us; 37us; 40us; 45us; 50us; 52us; 54us; 59us; 60us; 65us; 66us; 71us; 72us; 73us; 78us; |]
let _fsyacc_reductionSymbolCounts = [|1us; 2us; 3us; 3us; 1us; 3us; 3us; 1us; 3us; 1us; 2us; 2us; 1us; 3us; |]
let _fsyacc_productionToNonTerminalTable = [|0us; 1us; 2us; 2us; 2us; 3us; 3us; 3us; 4us; 4us; 5us; 5us; 5us; 5us; |]
let _fsyacc_immediateActions = [|65535us; 49152us; 65535us; 16385us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16393us; 65535us; 16394us; 65535us; 16395us; 16396us; 65535us; 16397us; |]
let _fsyacc_reductions ()  =    [| 
# 131 "/Users/kevinmoore/Documents/DTU/Datalogisk Modellering/FSharp/mandatory-assignment-master-303b1e1a5ced82e14a9bce5662a3be036aaf73d7/calculator/CalculatorParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startstart));
# 140 "/Users/kevinmoore/Documents/DTU/Datalogisk Modellering/FSharp/mandatory-assignment-master-303b1e1a5ced82e14a9bce5662a3be036aaf73d7/calculator/CalculatorParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 36 "/Users/kevinmoore/Documents/DTU/Datalogisk Modellering/FSharp/mandatory-assignment-master-303b1e1a5ced82e14a9bce5662a3be036aaf73d7/calculator/CalculatorParser.fsp"
                                                          _1 
                   )
# 36 "/Users/kevinmoore/Documents/DTU/Datalogisk Modellering/FSharp/mandatory-assignment-master-303b1e1a5ced82e14a9bce5662a3be036aaf73d7/calculator/CalculatorParser.fsp"
                 : expr));
# 151 "/Users/kevinmoore/Documents/DTU/Datalogisk Modellering/FSharp/mandatory-assignment-master-303b1e1a5ced82e14a9bce5662a3be036aaf73d7/calculator/CalculatorParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 48 "/Users/kevinmoore/Documents/DTU/Datalogisk Modellering/FSharp/mandatory-assignment-master-303b1e1a5ced82e14a9bce5662a3be036aaf73d7/calculator/CalculatorParser.fsp"
                                                           PlusExpr(_1,_3) 
                   )
# 48 "/Users/kevinmoore/Documents/DTU/Datalogisk Modellering/FSharp/mandatory-assignment-master-303b1e1a5ced82e14a9bce5662a3be036aaf73d7/calculator/CalculatorParser.fsp"
                 : expr));
# 163 "/Users/kevinmoore/Documents/DTU/Datalogisk Modellering/FSharp/mandatory-assignment-master-303b1e1a5ced82e14a9bce5662a3be036aaf73d7/calculator/CalculatorParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 49 "/Users/kevinmoore/Documents/DTU/Datalogisk Modellering/FSharp/mandatory-assignment-master-303b1e1a5ced82e14a9bce5662a3be036aaf73d7/calculator/CalculatorParser.fsp"
                                                           MinusExpr(_1,_3) 
                   )
# 49 "/Users/kevinmoore/Documents/DTU/Datalogisk Modellering/FSharp/mandatory-assignment-master-303b1e1a5ced82e14a9bce5662a3be036aaf73d7/calculator/CalculatorParser.fsp"
                 : expr));
# 175 "/Users/kevinmoore/Documents/DTU/Datalogisk Modellering/FSharp/mandatory-assignment-master-303b1e1a5ced82e14a9bce5662a3be036aaf73d7/calculator/CalculatorParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 50 "/Users/kevinmoore/Documents/DTU/Datalogisk Modellering/FSharp/mandatory-assignment-master-303b1e1a5ced82e14a9bce5662a3be036aaf73d7/calculator/CalculatorParser.fsp"
                                                           _1 
                   )
# 50 "/Users/kevinmoore/Documents/DTU/Datalogisk Modellering/FSharp/mandatory-assignment-master-303b1e1a5ced82e14a9bce5662a3be036aaf73d7/calculator/CalculatorParser.fsp"
                 : expr));
# 186 "/Users/kevinmoore/Documents/DTU/Datalogisk Modellering/FSharp/mandatory-assignment-master-303b1e1a5ced82e14a9bce5662a3be036aaf73d7/calculator/CalculatorParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 54 "/Users/kevinmoore/Documents/DTU/Datalogisk Modellering/FSharp/mandatory-assignment-master-303b1e1a5ced82e14a9bce5662a3be036aaf73d7/calculator/CalculatorParser.fsp"
                                                           TimesExpr(_1,_3) 
                   )
# 54 "/Users/kevinmoore/Documents/DTU/Datalogisk Modellering/FSharp/mandatory-assignment-master-303b1e1a5ced82e14a9bce5662a3be036aaf73d7/calculator/CalculatorParser.fsp"
                 : expr));
# 198 "/Users/kevinmoore/Documents/DTU/Datalogisk Modellering/FSharp/mandatory-assignment-master-303b1e1a5ced82e14a9bce5662a3be036aaf73d7/calculator/CalculatorParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 55 "/Users/kevinmoore/Documents/DTU/Datalogisk Modellering/FSharp/mandatory-assignment-master-303b1e1a5ced82e14a9bce5662a3be036aaf73d7/calculator/CalculatorParser.fsp"
                                                           DivExpr(_1,_3) 
                   )
# 55 "/Users/kevinmoore/Documents/DTU/Datalogisk Modellering/FSharp/mandatory-assignment-master-303b1e1a5ced82e14a9bce5662a3be036aaf73d7/calculator/CalculatorParser.fsp"
                 : expr));
# 210 "/Users/kevinmoore/Documents/DTU/Datalogisk Modellering/FSharp/mandatory-assignment-master-303b1e1a5ced82e14a9bce5662a3be036aaf73d7/calculator/CalculatorParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 56 "/Users/kevinmoore/Documents/DTU/Datalogisk Modellering/FSharp/mandatory-assignment-master-303b1e1a5ced82e14a9bce5662a3be036aaf73d7/calculator/CalculatorParser.fsp"
                                                           _1 
                   )
# 56 "/Users/kevinmoore/Documents/DTU/Datalogisk Modellering/FSharp/mandatory-assignment-master-303b1e1a5ced82e14a9bce5662a3be036aaf73d7/calculator/CalculatorParser.fsp"
                 : expr));
# 221 "/Users/kevinmoore/Documents/DTU/Datalogisk Modellering/FSharp/mandatory-assignment-master-303b1e1a5ced82e14a9bce5662a3be036aaf73d7/calculator/CalculatorParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 60 "/Users/kevinmoore/Documents/DTU/Datalogisk Modellering/FSharp/mandatory-assignment-master-303b1e1a5ced82e14a9bce5662a3be036aaf73d7/calculator/CalculatorParser.fsp"
                                                           PowExpr(_1,_3) 
                   )
# 60 "/Users/kevinmoore/Documents/DTU/Datalogisk Modellering/FSharp/mandatory-assignment-master-303b1e1a5ced82e14a9bce5662a3be036aaf73d7/calculator/CalculatorParser.fsp"
                 : expr));
# 233 "/Users/kevinmoore/Documents/DTU/Datalogisk Modellering/FSharp/mandatory-assignment-master-303b1e1a5ced82e14a9bce5662a3be036aaf73d7/calculator/CalculatorParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 61 "/Users/kevinmoore/Documents/DTU/Datalogisk Modellering/FSharp/mandatory-assignment-master-303b1e1a5ced82e14a9bce5662a3be036aaf73d7/calculator/CalculatorParser.fsp"
                                           _1 
                   )
# 61 "/Users/kevinmoore/Documents/DTU/Datalogisk Modellering/FSharp/mandatory-assignment-master-303b1e1a5ced82e14a9bce5662a3be036aaf73d7/calculator/CalculatorParser.fsp"
                 : expr));
# 244 "/Users/kevinmoore/Documents/DTU/Datalogisk Modellering/FSharp/mandatory-assignment-master-303b1e1a5ced82e14a9bce5662a3be036aaf73d7/calculator/CalculatorParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 65 "/Users/kevinmoore/Documents/DTU/Datalogisk Modellering/FSharp/mandatory-assignment-master-303b1e1a5ced82e14a9bce5662a3be036aaf73d7/calculator/CalculatorParser.fsp"
                                                          UPlusExpr(_2) 
                   )
# 65 "/Users/kevinmoore/Documents/DTU/Datalogisk Modellering/FSharp/mandatory-assignment-master-303b1e1a5ced82e14a9bce5662a3be036aaf73d7/calculator/CalculatorParser.fsp"
                 : expr));
# 255 "/Users/kevinmoore/Documents/DTU/Datalogisk Modellering/FSharp/mandatory-assignment-master-303b1e1a5ced82e14a9bce5662a3be036aaf73d7/calculator/CalculatorParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 66 "/Users/kevinmoore/Documents/DTU/Datalogisk Modellering/FSharp/mandatory-assignment-master-303b1e1a5ced82e14a9bce5662a3be036aaf73d7/calculator/CalculatorParser.fsp"
                                                          UMinusExpr(_2) 
                   )
# 66 "/Users/kevinmoore/Documents/DTU/Datalogisk Modellering/FSharp/mandatory-assignment-master-303b1e1a5ced82e14a9bce5662a3be036aaf73d7/calculator/CalculatorParser.fsp"
                 : expr));
# 266 "/Users/kevinmoore/Documents/DTU/Datalogisk Modellering/FSharp/mandatory-assignment-master-303b1e1a5ced82e14a9bce5662a3be036aaf73d7/calculator/CalculatorParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : float)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 67 "/Users/kevinmoore/Documents/DTU/Datalogisk Modellering/FSharp/mandatory-assignment-master-303b1e1a5ced82e14a9bce5662a3be036aaf73d7/calculator/CalculatorParser.fsp"
                                                          Num(_1) 
                   )
# 67 "/Users/kevinmoore/Documents/DTU/Datalogisk Modellering/FSharp/mandatory-assignment-master-303b1e1a5ced82e14a9bce5662a3be036aaf73d7/calculator/CalculatorParser.fsp"
                 : expr));
# 277 "/Users/kevinmoore/Documents/DTU/Datalogisk Modellering/FSharp/mandatory-assignment-master-303b1e1a5ced82e14a9bce5662a3be036aaf73d7/calculator/CalculatorParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 68 "/Users/kevinmoore/Documents/DTU/Datalogisk Modellering/FSharp/mandatory-assignment-master-303b1e1a5ced82e14a9bce5662a3be036aaf73d7/calculator/CalculatorParser.fsp"
                                                          _2 
                   )
# 68 "/Users/kevinmoore/Documents/DTU/Datalogisk Modellering/FSharp/mandatory-assignment-master-303b1e1a5ced82e14a9bce5662a3be036aaf73d7/calculator/CalculatorParser.fsp"
                 : expr));
|]
# 289 "/Users/kevinmoore/Documents/DTU/Datalogisk Modellering/FSharp/mandatory-assignment-master-303b1e1a5ced82e14a9bce5662a3be036aaf73d7/calculator/CalculatorParser.fs"
let tables () : FSharp.Text.Parsing.Tables<_> = 
  { reductions= _fsyacc_reductions ();
    endOfInputTag = _fsyacc_endOfInputTag;
    tagOfToken = tagOfToken;
    dataOfToken = _fsyacc_dataOfToken; 
    actionTableElements = _fsyacc_actionTableElements;
    actionTableRowOffsets = _fsyacc_actionTableRowOffsets;
    stateToProdIdxsTableElements = _fsyacc_stateToProdIdxsTableElements;
    stateToProdIdxsTableRowOffsets = _fsyacc_stateToProdIdxsTableRowOffsets;
    reductionSymbolCounts = _fsyacc_reductionSymbolCounts;
    immediateActions = _fsyacc_immediateActions;
    gotos = _fsyacc_gotos;
    sparseGotoTableRowOffsets = _fsyacc_sparseGotoTableRowOffsets;
    tagOfErrorTerminal = _fsyacc_tagOfErrorTerminal;
    parseError = (fun (ctxt:FSharp.Text.Parsing.ParseErrorContext<_>) -> 
                              match parse_error_rich with 
                              | Some f -> f ctxt
                              | None -> parse_error ctxt.Message);
    numTerminals = 12;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = (tables ()).Interpret(lexer, lexbuf, startState)
let start lexer lexbuf : expr =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 0))