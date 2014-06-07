type expression =
    Atom of string
  | Slist of expression list

(* Character position for error reporting. *)
let position = ref 0

(* The reader uses one character of lookahead.  There is no way to put a
   character back to an input channel, so we keep it here. *)
let lookahead = ref None

(* Peek at the next character. *)
let peek_char chan =
  match !lookahead with
    | None ->
      let c = input_char chan in lookahead := Some c; c
    | Some c -> c

(* Read the next character. *)
let read_char chan =
  let c =
    match !lookahead with
      | None -> input_char chan
      | Some c -> (lookahead := None; c)
  in position := !position + 1; c

(* Skip spaces, tabs, and returns. *)
let rec skip_whitespace chan =
  match peek_char chan with
    | ' ' | '\n' | '\r' | '\t' ->
      let _ = read_char chan in skip_whitespace chan
    | _ -> ()

(* Consume exactly 'c'.  Fail if the next character is not 'c'. *)
let consume c chan =
  let c' = read_char chan in
  if (c' != c) then
    failwith ("expected " ^ Char.escaped c
	      ^ " got " ^ Char.escaped c'
	      ^ " at " ^ string_of_int (!position))

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
let read_atom chan =
  let rec loop acc =
    match peek_char chan with
      | '(' | ')' | ' ' | '\n' | '\r' | '\t' -> acc
      | c -> let _ = read_char chan in loop (c :: acc)
  in
  let atom = implode_reversed (loop []) in
  atom

(* Read a list of s-expressions from the input channel, and skip trailing
   whitespace. *)
let rec read_list chan =
  consume '(' chan;
  skip_whitespace chan;
  let rec recur () =
    match peek_char chan with
      | ')' -> let _ = read_char chan in []
      | c -> let sexp = read_sexp chan in sexp :: recur ()
  in
  let lst = recur () in
  lst

(* Read an s-expression from the channel, and skip trailing whitespace. *)
and read_sexp chan =
  skip_whitespace chan;
  match peek_char chan with
    | '(' -> Slist (read_list chan)
    | _ -> Atom (read_atom chan)
