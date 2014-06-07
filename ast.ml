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

(* ==== Construction from S-expressions ==== *)
(* The Sexp is assumed to be well-formed, otherwise a pattern match failure
   will occur. *)
open Sexp

let build_string = function
  | Atom s -> s
  | _ -> failwith "not an atom"

let build_string_list = function
  | Slist lst -> List.map build_string lst
  | _ -> failwith "not a list"

let bad_atom_msg kind atom = "bad " ^ kind ^ ": " ^ atom

let bad_slist_msg kind slist =
  "bad " ^ kind ^ ": list[" ^ string_of_int (List.length slist) ^ "]"

let rec build_expression = function
  | Slist [Atom "Constant"; Atom value] ->
    Constant (int_of_string value)
  | Slist [Atom "Variable"; Atom name] ->
    Variable name
  | Slist [Atom "Assignment"; Atom name; expr] ->
    Assignment (name, build_expression expr)
  | Slist [Atom "Call"; Atom name; args] ->
    Call (name, build_expression_list args)
  | Slist [Atom "Throw"; expr] ->
    Throw (build_expression expr)
  | Slist [Atom "Await"; expr] ->
    Await (build_expression expr)
  | Slist (Atom tag :: _) -> failwith ("bad list tagged: " ^ tag)
  | Slist slist -> failwith (bad_slist_msg "expression" slist)
  | Atom atom -> failwith (bad_atom_msg "expression" atom)

and build_expression_list = function
  | Slist lst -> List.map build_expression lst
  | _ -> failwith "not a list"

let rec build_statement = function
  | Atom "NoStatement" -> NoStatement
  | Slist [Atom "Expression"; expr; next] ->
    Expression (build_expression expr, build_statement next)
  | Slist [Atom "Return"; expr; next] ->
    Return (build_expression expr, build_statement next)
  | Slist [Atom "YieldBreak"; next] ->
    YieldBreak (build_statement next)
  | Slist [Atom "Yield"; expr; next] ->
    Yield (build_expression expr, build_statement next)
  | Slist [Atom "YieldStar"; expr; next] ->
    YieldStar (build_expression expr, build_statement next)
  | Slist [Atom "If"; cond; thn; els; next] ->
    If (build_expression cond, build_statement thn, build_statement els,
        build_statement next)
  | Slist [Atom "Label"; Atom label; stmt; next] ->
    Label (label, build_statement stmt, build_statement next)
  | Slist [Atom "Break"; Atom label; next] ->
    Break (label, build_statement next)
  | Slist [Atom "While"; Atom label; cond; body; next] ->
    While (label, build_expression cond, build_statement body,
           build_statement next)
  | Slist [Atom "Continue"; Atom label; next] ->
    Continue (label, build_statement next)
  | Slist [Atom "TryCatch"; body; Atom exn; catch; next] ->
    TryCatch (build_statement body, exn, build_statement catch,
              build_statement next)
  | Slist [Atom "TryFinally"; body; finally; next] ->
    TryFinally (build_statement body, build_statement finally,
                build_statement next)
  | Slist slist -> failwith (bad_slist_msg "statement" slist)
  | Atom atom -> failwith (bad_atom_msg "statement" atom)

let build_function_declaration = function
  | Slist [Atom tag; Atom name; parameters; locals; body] ->
    let parameters = build_string_list parameters in
    let locals = build_string_list locals in
    let body = build_statement body in
    (match tag with
      | "Sync" -> Sync (name, parameters, locals, body)
      | "SyncStar" -> SyncStar (name, parameters, locals, body)
      | "Async" -> Async (name, parameters, locals, body)
      | _ -> failwith ("unexpected tag " ^ tag))
  | Slist slist -> failwith (bad_slist_msg "function declaration" slist)
  | Atom atom -> failwith (bad_atom_msg "function declaration" atom)

(* Read an S-expression from standard in, return a list of Asts. *)
let read_asts chan =
  match Sexp.read_sexp chan with
  | Slist slist -> List.map build_function_declaration slist
  | Atom atom -> failwith (bad_atom_msg "compilation unit" atom)
