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

let expression_stmt expr rest = Expression (expr, rest)
let return_stmt expr rest = Return (expr, rest)
let yield_break_stmt next = YieldBreak next
let yield_stmt expr next = Yield (expr, next)
let yield_star_stmt expr next = YieldStar (expr, next)
let if_stmt expr thn els rest = If (expr, thn, els, rest)
let label_stmt label stmt rest = Label (label, stmt, rest)
let break_stmt label rest = Break (label, rest)
let while_stmt label expr body rest = While (label, expr, body, rest)
let continue_stmt label rest = Continue (label, rest)
let try_catch_stmt stmt id catch_stmt rest = TryCatch (stmt, id, catch_stmt, rest)
let try_finally_stmt stmt finally_stmt rest = TryFinally (stmt, finally_stmt, rest)

let block stmts =
  List.fold_right (fun stmt acc -> stmt acc) stmts NoStatement
