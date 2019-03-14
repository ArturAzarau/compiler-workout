open GT       
open Language
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)                         
let evaluateInstruction statementConfiguration instruction =
  let (stack, configuration) = statementConfiguration in
  let (state, inputStream, outputStream) = configuration in

  match instruction with
    | BINOP operation -> (match stack with
      | y::x::left -> [(Language.Expr.evaluateOperation operation) x y] @ left, configuration
      | _ -> failwith("Error"))
    | CONST value -> [value] @ stack, configuration
    | READ -> (match inputStream with 
      | input::left -> [input] @ stack, (state, left, outputStream)
      | _ -> failwith("Error"))
    | WRITE -> (match stack with 
      | value::left -> left, (state, inputStream, outputStream @ [value])
      | _ -> failwith("Error"))
    | LD variable -> ([state variable] @ stack, configuration)
    | ST variable -> (match stack with 
      | value::left -> (left, (Language.Expr.update variable value state, inputStream, outputStream))
      | _ -> failwith("Error"));;

let eval configuration programm = List.fold_left evaluateInstruction configuration programm

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i = let (_, (_, _, o)) = eval ([], (Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
*)
let rec compile =
  let rec expr = function
  | Expr.Var   x          -> [LD x]
  | Expr.Const n          -> [CONST n]
  | Expr.Binop (op, x, y) -> expr x @ expr y @ [BINOP op]
  in
  function
  | Stmt.Seq (s1, s2)  -> compile s1 @ compile s2
  | Stmt.Read x        -> [READ; ST x]
  | Stmt.Write e       -> expr e @ [WRITE]
  | Stmt.Assign (x, e) -> expr e @ [ST x]

let rec compile statement = match statement with
  | Language.Stmt.Read variable -> [READ; ST variable]
  | Language.Stmt.Write expression -> (compileExpression expression) @ [WRITE]
  | Language.Stmt.Assign (variable, expression) -> (compileExpression expression) @ [ST variable]
  | Language.Stmt.Seq (first, second) -> (compile first) @ (compile second);;
