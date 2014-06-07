type expression =
    Atom of string
  | Slist of expression list

(* Pass an input channel, e.g., 'stdin' or 'open_in "filename"'. *)
val read_sexp : in_channel -> expression
