open Ast

type s_expression =
    Atom of string
  | Slist of s_expression list

(* Change this to 'open_in "filename"' to read from a file. *)
let in_channel = stdin (* open_in "test.s" *)

(* Character position for error reporting. *)
let position = ref 0

(* The reader uses one character of lookahead.  There is no way to put a
   character back to an in_channel, so we keep it here. *)
let lookahead = ref None

(* Peek at the next character. *)
let peek_char () =
  match !lookahead with
    | None ->
      let c = input_char in_channel in lookahead := Some c; c
    | Some c -> c

(* Read the next character. *)
let read_char () =
  let c =
    match !lookahead with
      | None -> input_char in_channel
      | Some c -> (lookahead := None; c)
  in position := !position + 1; c

(* Skip spaces, tabs, and returns. *)
let rec skip_whitespace () =
  match peek_char () with
    | ' ' | '\n' | '\r' | '\t' -> let _ = read_char () in skip_whitespace ()
    | _ -> ()

(* Consume exactly 'c'.  Fail if the next character is not 'c'. *)
let consume c =
  let c' = read_char () in
  if (c' != c) then
    let msg = "expected " ^ Char.escaped c
      ^ " got " ^ Char.escaped c'
      ^ " at " ^ string_of_int (!position)
    in ignore (failwith msg)

(* Reverse a list of characters and convert to a string. *)
let implode_reversed cs =
  let len = List.length cs in
  let result = String.create len in
  let rec loop cs i =
    match cs with
      | [] -> result
      | c :: cs -> result.[i] <- c; loop cs (i - 1)
  in
  loop cs (len - 1)

(* Read a string from the input channel, delimited by whitespace or parens;
   and skip trailing whitespace. *)
let read_atom () =
  let rec loop acc =
    match peek_char () with
      | '(' | ')' | ' ' | '\n' | '\r' | '\t' -> acc
      | c -> let _ = read_char () in loop (c :: acc)
  in
  let atom = implode_reversed (loop []) in
  skip_whitespace ();
  atom

(* Read a list of s-expressions from the input channel, and skip trailing
   whitespace. *)
let rec read_list () =
  consume '(';
  skip_whitespace ();
  let rec recur () =
    match peek_char () with
      | ')' -> let _ = read_char () in []
      | c -> let sexp = read_sexp () in sexp :: recur ()
  in
  let lst = recur () in
  skip_whitespace ();
  lst

(* Read an s-expression from the channel, and skip trailing whitespace. *)
and read_sexp () =
  match peek_char () with
    | '(' -> Slist (read_list ())
    | _ -> Atom (read_atom ())

(* Read a list of s-expressions after skipping initial whitespace. *)
let read_stream () =
  skip_whitespace ();
  read_list ()


(* ==== Construct an Ast from an Sexp. ==== *)

(* The Sexp is assumed to be well-formed, otherwise a pattern match failure
   will occur. *)
let build_string = function
  | Atom s -> s

let build_string_list = function
  | Slist [] -> []
  | Slist lst -> List.map build_string lst

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
  | Atom x -> failwith x
  | Slist (Atom x :: _) -> failwith x
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
  | Atom x -> failwith x
  | Slist (Atom x :: _) -> failwith x

let build_function_declaration = function
  | Slist [Atom tag; Atom name; parameters; locals; body] ->
    let parameters = build_string_list parameters in
    let locals = build_string_list locals in
    let body = build_statement body in
    (match tag with
      | "Sync" -> Sync (name, parameters, locals, body)
      | "SyncStar" -> SyncStar (name, parameters, locals, body)
      | "Async" -> Async (name, parameters, locals, body))

(* Read an S-expression from standard in, return a list of Asts. *)
let build_compilation_unit () =
  List.map build_function_declaration (read_stream ())
