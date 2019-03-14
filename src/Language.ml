open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap.Combinators
       
(* Simple expressions: syntax and semantics *)
module Expr =
  struct
    
    (* The type for expressions. Note, in regular OCaml there is no "@type..." 
       notation, it came from GT. 
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)
                                                            
    (* State: a partial map from variables to integer values. *)
    type state = string -> int 

    (* Empty state: maps every variable into nothing. *)
    let empty = fun x -> failwith (Printf.sprintf "Undefined variable %s" x)

    (* Update: non-destructively "modifies" the state s by binding the variable x 
      to value v and returns the new state.
    *)
    let update x v s = fun y -> if x = y then v else s y

    (* Convert int to bool *)
    let convertIntToBool value = value != 0;;

    (* Convert bool to int *)
    let convertBoolToInt value = if value then 1 else 0;;

    (* Evaluate operation *)
    let evaluateOperation operation left right = match operation with
      | "+" -> left + right
      | "-" -> left - right
      | "*" -> left * right
      | "/" -> left / right
      | "%" -> left mod right
      | "<" -> convertBoolToInt (left < right)
      | ">" -> convertBoolToInt (left > right)
      | "<=" -> convertBoolToInt (left <= right)
      | ">=" -> convertBoolToInt (left >= right)
      | "==" -> convertBoolToInt (left == right)
      | "!=" -> convertBoolToInt (left != right)
      | "&&" -> convertBoolToInt (convertIntToBool left && convertIntToBool right)
      | "!!" -> convertBoolToInt (convertIntToBool left || convertIntToBool right)
      | _ -> failwith "Error";;

    (* Expression evaluator

          val eval : state -> t -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
    *)
    let rec eval state expression = match expression with
      | Const value -> value
      | Var variable -> state variable
      | Binop(operation, left, right) -> evaluateOperation operation (eval state left) (eval state right);;


    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
   
    *)

    let parseBinOperator operator = ostap(- $(operator)), (fun left right -> Binop(operator, left, right))

    ostap (
      expr:
        !(Ostap.Util.expr(fun x -> x)
          (Array.map (fun (asc, ops) -> asc, List.map parseBinOperator ops)
          [|
              `Lefta, ["!!"];
              `Lefta, ["&&"];
              `Nona, ["<="; "<"; ">="; ">"; "=="; "!="];
              `Lefta, ["+"; "-"];
              `Lefta, ["*"; "/"; "%"];
          
          |]
          )
          primary
        );
        primary: variable:IDENT { Var variable } | const: DECIMAL { Const const } | -"(" expr -")"
    )

  end
                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t with show

    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list 

    (* Statement evaluator

          val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)
    let rec eval configuration statement = 
      let (state, inputStream, outputStream) = configuration in
      match statement with
        | Read variable -> (match inputStream with 
          | value::left -> (Expr.update variable value state), left, outputStream
          | [] -> failwith "Empty input")
        | Write expression -> (state, inputStream, Expr.eval state expression :: outputStream)
        | Assign (variable, expression) -> (Expr.update variable (Expr.eval state expression) state), inputStream, outputStream
        | Seq (first, second) -> eval (eval configuration first) second;;

    (* Statement parser *)
    
      ostap (
	      statement:
	        x:IDENT ":=" e:!(Expr.expr)         {Assign (x, e)}
	        | "read"  "("  x:IDENT        ")"   {Read x}
	        | "write" "("  e:!(Expr.expr) ")"   {Write e};
	      parse: l:statement ";" rest:parse {Seq (l, rest)} | statement
	    )
  end

(* The top-level definitions *)

(* The top-level syntax category is statement *)
type t = Stmt.t    

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval p i =
  let _, _, o = Stmt.eval (Expr.empty, i, []) p in o
