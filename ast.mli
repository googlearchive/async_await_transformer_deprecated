type expression =
  | Constant of int
  | Variable of string
  | Assignment of string * expression
  | Call of string * expression list
  | Throw of expression
  | Await of expression
      
type statement =
  | NoStatement
  | Expression of expression * statement
  | Return of expression * statement
  | YieldBreak of statement  (* Return in a sync* body. *)
  | Yield of expression * statement
  | YieldStar of expression * statement
  | If of expression * statement * statement * statement
  | Label of string * statement * statement
  | Break of string * statement
  | While of string * expression * statement * statement
  | Continue of string * statement
  | TryCatch of statement * string * statement * statement
  | TryFinally of statement * statement * statement
  (* | Switch of expression * cases *)
  (* and cases = *)
  (* | NoCases *)
  (* | Case of string * int * statement * cases *)

type function_declaration =
  | Sync of string * string list * string list * statement
  | SyncStar of string * string list * string list * statement
  | Async of string * string list * string list * statement


val expression_stmt : expression -> statement -> statement
val return_stmt : expression -> statement -> statement
val yield_break_stmt : statement -> statement
val yield_stmt : expression -> statement -> statement
val yield_star_stmt : expression -> statement -> statement
val if_stmt : expression -> statement -> statement -> statement -> statement
val label_stmt : string -> statement -> statement -> statement
val break_stmt : string -> statement -> statement
val while_stmt : string -> expression -> statement -> statement -> statement
val continue_stmt : string -> statement -> statement
val try_catch_stmt : statement -> string -> statement -> statement -> statement
val try_finally_stmt : statement -> statement -> statement -> statement

val block : (statement -> statement) list -> statement

(* Pass an input channel, e.g., 'stdin' or 'open_in "filename"'. *)
val read_asts : in_channel -> function_declaration list
