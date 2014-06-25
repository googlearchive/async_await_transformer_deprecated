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
  let msg =
    "bad " ^ kind ^ ": list[" ^ string_of_int (List.length slist) ^ "]" in
  match slist with
    | Atom tag :: _ -> msg ^ " tagged: " ^ tag
    | _ -> msg

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
  | Slist slist -> failwith (bad_slist_msg "expression" slist)
  | Atom atom -> failwith (bad_atom_msg "expression" atom)

and build_expression_list = function
  | Slist lst -> List.map build_expression lst
  | _ -> failwith "not a list"

let rec build_statement = function
  | Atom "EmptyStatement" -> EmptyStatement
  | Slist [Atom "Block"; statements] -> Block (build_statement_list statements)
  | Slist [Atom "Expression"; expr] -> Expression (build_expression expr)
  | Slist [Atom "Return"; expr] -> Return (build_expression expr)
  | Slist [Atom "If"; cond; thn; els] ->
    If (build_expression cond, build_statement thn, build_statement els)
  | Slist [Atom "Label"; Atom label; stmt] -> Label (label, build_statement stmt)
  | Slist [Atom "Break"; Atom label] -> Break (label)
  | Slist [Atom "While"; Atom label; cond; body] ->
    While (label, build_expression cond, build_statement body)
  | Slist [Atom "Continue"; Atom label] -> Continue (label)
  | Slist [Atom "TryCatch"; body; Atom exn; catch] ->
    TryCatch (build_statement body, exn, build_statement catch)
  | Slist [Atom "TryFinally"; body; finally] ->
    TryFinally (build_statement body, build_statement finally)
  | Slist slist -> failwith (bad_slist_msg "statement" slist)
  | Atom atom -> failwith (bad_atom_msg "statement" atom)

and build_statement_list = function
  | Slist lst -> List.map build_statement lst
  | _ -> failwith "not a list"

let build_function_declaration = function
  | Slist [Atom tag; Atom name; parameters; locals; body] ->
    let parameters = build_string_list parameters in
    let locals = build_string_list locals in
    let body = build_statement body in
    (match tag with
      | "Sync" -> Sync (name, parameters, locals, body)
      | "Async" -> Async (name, parameters, locals, body)
      | _ -> failwith ("unexpected tag " ^ tag))
  | Slist slist -> failwith (bad_slist_msg "declaration" slist)
  | Atom atom -> failwith (bad_atom_msg "declaration" atom)

(* Read an S-expression from standard in, return a list of Asts. *)
let read_asts chan =
  match Sexp.read_sexp chan with
  | Slist slist -> List.map build_function_declaration slist
  | Atom atom -> failwith (bad_atom_msg "compilation unit" atom)
