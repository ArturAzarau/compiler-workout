open GT       
       
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
type config = int list * Syntax.Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)                         

let evaluateInstruction statementConfiguration instruction =
  let (stack, configuration) = statementConfiguration in
  let (state, inputStream, outputStream) = configuration in

  match instruction with
    | BINOP operation -> (match stack with
      | y::x::left -> [Syntax.Expr.evaluateOperation operation x y] @ left, configuration)
    
    | CONST value -> [value] @ stack, configuration
    | READ -> (match inputStream with | input::left -> [input] @ stack, (state, left, outputStream))
    | WRITE -> (match stack with | value::left -> left, (state, inputStream, outputStream @ [value]))
    | LD variable -> [state variable] @ stack, configuration
    | ST variable -> (match stack with value::left -> left, (Syntax.Expr.update variable value state, inputStream, outputStream))

let eval configuration, programm = List.fold_left evaluateInstruction configuration programm

(* Top-level evaluation

     val run : int list -> prg -> int list

   Takes an input stream, a program, and returns an output stream this program calculates
*)
let run i p = let (_, (_, _, o)) = eval ([], (Syntax.Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Syntax.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)

let rec compileExpression expression = match expression with
  | Syntax.Expr.Const value -> [CONST value]
  | Syntax.Expr.Var variable -> [LD variable]
  | Syntax.Expr.Binop (operator, lhs, rhs) -> (compileExpression lhs) @ (compileExpression rhs) @ [BINOP operator]

let compile statement = match statement with
  | Syntax.Stmt.Read variable -> [READ; ST variable]
  | Syntax.Stmt.Write expression -> (compileExpression expression) @ [WRITE]
  | Syntax.Stmt.Assign (variable, expression) -> (compileExpression expression) @ [ST variable]
  | Syntax.Stmt.Seq (first, second) -> (compile first) @ (compile second) 
