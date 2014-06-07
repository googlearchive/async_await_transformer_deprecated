type expression =
    Atom of string
  | Slist of expression list

(* Pass an input channel, e.g., 'stdin' or 'open_in "filename"'. *)
val read_sexp : in_channel -> expression

(* Pass an output chanel, e.g., 'stdout' or 'open_out "filename"'. *)
val write_sexp : out_channel -> expression -> unit
