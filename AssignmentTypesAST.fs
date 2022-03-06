// This file implements a module where we define a data type "expr"
// to store represent arithmetic expressions

// C ::= x := a | A[a] := a | skip | C ; C | if GC fi | do GC od
// GC ::= b -> C | GC [] GC
// a ::= n | x | A[a] | a + a | a - a | a * a | a / a | - a | a ^ a | (a)
// b ::= true | false | b & b | b | b | b && b | b || b | !b
// | a = a | a != a | a > a | a >= a | a < a | a <= a | (b)

module AssignmentTypesAST

type aExpr =
  | Num of float
  | Var of String
  | ListAExpr of (String * aExpr)
  | TimesExpr of (aExpr * aExpr)
  | DivExpr of (aExpr * aExpr)
  | PlusExpr of (aExpr * aExpr)
  | MinusExpr of (aExpr * aExpr)
  | PowExpr of (aExpr * aExpr)
  | UPlusExpr of (aExpr)
  | UMinusExpr of (aExpr)

type bExpr =
  | True
  | False
  | AndExpr of (bExpr * bExpr)   // &
  | OrExpr of (bExpr * bExpr)    // |
  | SCAndExpr of (bExpr * bExpr) // Short Circuit And &&
  | SCOrExpr of (bExpr * bExpr)  // Short Circuit Or  ||
  | NotExpr of (bExpr)
  | EqExpr of (aExpr * aExpr)
  | NotEqExpr of (aExpr * aExpr)
  | GrExpr of (aExpr * aExpr)
  | GrEqExpr of (aExpr * aExpr)
  | LeExpr of (aExpr * aExpr)
  | LeEqExpr of (aExpr * aExpr)

type command =
  | AssignVarExpr of (String * aExpr)
  | AssignArray of (String * aExpr * aExpr) // First: which array, second: index, third: value
  | Skip
  | CommandSeq of (command * command)
  | IfExpr of (guardedCommands)
  | DoExpr of (guardedCommands)

and guardedCommands =
  | BoolGC of (bExpr * command)  // bool -> command
  | GCSequence of (guardedCommands * guardedCommands)