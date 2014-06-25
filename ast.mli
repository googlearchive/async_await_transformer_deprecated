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
  | If of expression * statement * statement
  | Label of string * statement
  | Break of string
  | While of string * expression * statement
  | Continue of string
  | TryCatch of statement * string * statement
  | TryFinally of statement * statement

type function_declaration =
  | Sync of string * string list * string list * statement
  | Async of string * string list * string list * statement

(* Pass an input channel, e.g., 'stdin' or 'open_in "filename"'. *)
val read_asts : in_channel -> function_declaration list

