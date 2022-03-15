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
  | Var of string
  | ListAExpr of (string * aExpr)
  | TimesExpr of (aExpr * aExpr)
  | DivExpr of (aExpr * aExpr)
  | PlusExpr of (aExpr * aExpr)
  | MinusExpr of (aExpr * aExpr)
  | PowExpr of (aExpr * aExpr)
  | UPlusExpr of (aExpr)
  | UMinusExpr of (aExpr)

type bExpr =
  | BoolExpr of bool // true or false
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
  | AssignVarExpr of (string * aExpr)
  | AssignArray of (string * aExpr * aExpr) // First: which array, second: index, third: value
  | Skip
  | CommandSeq of (command * command)
  | IfExpr of (guardedCommand)
  | DoExpr of (guardedCommand)

and guardedCommand =
  | BoolGC of (bExpr * command)  // bool -> command
  | GCSequence of (guardedCommand * guardedCommand)