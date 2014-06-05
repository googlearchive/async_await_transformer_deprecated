type value =
  | Constant of int
  | Fun of string list * string * expression
    (* Values used to implement yield and yield*. *)
  | Done
  | Single of string * string
  | Nested of string * string
    (* Values used to implement await. *)
  | Await of string * string * string
      
and expression =
  | LetVal of string * value * expression
  | LetCont of string * string list * expression * expression
  | CallFun of string * string list * string
  | CallCont of string * string list
  | If of string * string * string

type function_declaration =
  | FunDecl of string * string list * string * string * expression
