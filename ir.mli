type value =
  | Constant of int
  | Fun of string list * string * string * expression
    (* Values used to implement await. *)
  | Await of string * string * string
      
and expression =
  | LetVal of string * value * expression
  | LetCont of string * string list * expression * expression
  | CallFun of string * string list * string * string
  | CallCont of string * string list
  | If of string * expression * expression

type function_declaration =
  | FunDecl of string * string list * string * string * expression

(* Pass an output channel, e.g., 'stdout' or 'open_out "filename"'. *)
val write_ir : out_channel -> function_declaration list -> unit
