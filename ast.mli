type expression =
  | Constant of int
  | Variable of string
  | Assignment of string * expression
  | Call of string * expression list
  | Throw of expression
  | Await of expression
      
type statement =
  | EmptyStatement
  | Block of statement list
  | Expression of expression
  | Return of expression
  | YieldBreak  (* Return in a sync* body. *)
  | Yield of expression
  | YieldStar of expression
  | If of expression * statement * statement
  | Label of string * statement
  | Break of string
  | While of string * expression * statement
  | Continue of string
  | TryCatch of statement * string * statement
  | TryFinally of statement * statement
  (* | Switch of expression * cases *)
  (* and cases = *)
  (* | NoCases *)
  (* | Case of string * int * statement * cases *)

type function_declaration =
  | Sync of string * string list * string list * statement
  | SyncStar of string * string list * string list * statement
  | Async of string * string list * string list * statement

(* Pass an input channel, e.g., 'stdin' or 'open_in "filename"'. *)
val read_asts : in_channel -> function_declaration list
