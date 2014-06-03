module AST =
struct
  type expression =
  | Constant of int
  | Variable of string
  | Assignment of string * expression
  | Call of string * expression list
  | Throw of expression
  | Await of expression
  (* | Yield of expression *)

  type statement =
  | NoStatement
  | Expression of expression * statement
  | Return of expression * statement
  | Label of string * statement * statement
  | Break of string * statement
  | If of expression * statement * statement * statement
  | While of string * expression * statement * statement
  | Continue of string * statement
  | TryCatch of statement * string * statement * statement
  | TryFinally of statement * statement * statement
  (* | Switch of expression * cases *)
  (* and cases = *)
  (* | NoCases *)
  (* | Case of string * int * statement * cases *)

  type function_declaration =
    | Sync of string * string list * statement
    | Async of string * string list * statement
end

let expression_stmt expr rest = AST.Expression (expr, rest)
let return_stmt expr rest = AST.Return (expr, rest)
let label_stmt label stmt rest = AST.Label (label, stmt, rest)
let break_stmt label rest = AST.Break (label, rest)
let if_stmt expr thn els rest = AST.If (expr, thn, els, rest)
let while_stmt label expr body rest = AST.While (label, expr, body, rest)
let continue_stmt label rest = AST.Continue (label, rest)
let try_catch_stmt stmt id catch_stmt rest = AST.TryCatch (stmt, id, catch_stmt, rest)
let try_finally_stmt stmt finally_stmt rest = AST.TryFinally (stmt, finally_stmt, rest)

let block stmts =
  List.fold_right (fun stmt acc -> stmt acc) stmts AST.NoStatement

module IR =
struct
  type value =
  | Constant of int
  (* TODO(kmillikin): Assignments do not work in the presence of control flow. *)
  | Variable of string

  and expression =
  | LetVal of string * value * expression
  | LetCont of string * string list * expression * expression
  | CallFun of string * string list * string
  | CallCont of string * string list
  | If of string * string * string
  | Await of string * string * string

  type function_declaration =
    | FunDecl of string * string list * string * string * expression
end

let gensym_counter = ref 0

let reset_gensym () = gensym_counter := 0

let gensym base_name =
  let id = base_name ^ string_of_int (!gensym_counter) in
  (gensym_counter := !gensym_counter + 1; id)

let rec translate_e expr k ek =
  match expr with
  | AST.Constant n ->
    let v = gensym "v" in
    IR.LetVal (v, IR.Constant n, k v)
  | AST.Variable id ->
    k id
  | AST.Assignment (id, expr) ->
    translate_e expr
      (fun v -> IR.LetVal (id, IR.Variable v, k v))
      ek
  | AST.Call (name, args) ->
    let c = gensym "cont" in
    let v = gensym "v" in
    translate_args args
      (fun vs ->
        IR.LetCont (c, [v], k v, IR.CallFun (name, vs, c)))
      ek
  | AST.Throw expr ->
    translate_e expr ek ek
  | AST.Await expr ->
    let c = gensym "cont" in
    let x = gensym "v" in
    let throw = gensym "throw" in
    let e = gensym "e" in
    translate_e expr
      (fun v ->
        IR.LetCont (c, [x], k x,
          IR.LetCont (throw, [e], ek e,
            IR.Await (v, c, throw))))
      ek
and translate_args args k ek =
  match args with
    | [] -> k []
    | arg :: args ->
      translate_e arg
        (fun v -> translate_args args (fun vs -> k (v :: vs)) ek)
        ek

let mkcont name = fun () -> IR.CallCont (name, [])

let rec translate_s stmt k ek bks cks rk =
  match stmt with
  | AST.NoStatement ->
    k ()
  | AST.Expression (expr, next) ->
    translate_e expr
      (fun v -> translate_s next k ek bks cks rk) ek
  | AST.Return (expr, next) ->
    translate_e expr rk ek
  | AST.Label (label, stmt, next) ->
    let break = gensym "break" in
    IR.LetCont (break, [], translate_s next k ek bks cks rk,
      translate_s stmt (mkcont break) ek ((label, mkcont break) :: bks) cks rk)
  | AST.Break (label, next) ->
    List.assoc label bks ()
  | AST.If (expr, thn, els, next) ->
    let join = gensym "join" in
    let if_true = gensym "then" in
    let if_false = gensym "else" in
    translate_e expr
      (fun v ->
	IR.LetCont (join, [], translate_s next k ek bks cks rk,
          IR.LetCont (if_true, [], translate_s thn (mkcont join) ek bks cks rk,
            IR.LetCont (if_false, [], translate_s els (mkcont join) ek bks cks rk,
              IR.If (v, if_true, if_false)))))
      ek
  | AST.While (label, expr, stmt, next) ->
    let loop = gensym "continue" in
    let break = gensym "break" in
    let body = gensym "body" in
    translate_e expr
      (fun v ->
	IR.LetCont (loop, [],
		    IR.LetCont (break, [], translate_s next k ek bks cks rk,
		      IR.LetCont (body, [], translate_s stmt (mkcont loop) ek
			                      ((label, mkcont break) :: bks)
					      ((label, mkcont loop) :: cks)
				              rk,
		        IR.If (v, body, break))),
	  IR.CallCont (loop, [])))
      ek
  | AST.Continue (label, next) ->
    List.assoc label bks ()
  | AST.TryCatch (body, id, catch_stmt, next) ->
    let rest = gensym "rest" in
    let throw = gensym "throw" in
    let e = gensym "e" in
    IR.LetCont (rest, [], translate_s next k ek bks cks rk,
      IR.LetCont (throw, [e], translate_s catch_stmt (mkcont rest) ek bks cks rk,
        translate_s body (mkcont rest) (fun e -> IR.CallCont (throw, [e])) bks cks rk))
  | AST.TryFinally (body, finally_stmt, next) ->
    let rest = gensym "rest" in
    let rethrow = gensym "throw" in let e = gensym "e" in
    let return = gensym "return" in let v = gensym "v" in
    let b_names = List.map (fun _ -> gensym "break") bks in
    let c_names = List.map (fun _ -> gensym "continue") cks in
    let finally = gensym "finally" in let c = gensym "where" in let x = gensym "what" in
    let ek' = fun e -> IR.CallCont (finally, [rethrow; e]) in
    let new_cont = fun k -> fun () -> IR.CallCont (finally, [k; "_"]) in
    let new_break_conts = List.map new_cont b_names in
    let new_continue_conts = List.map new_cont c_names in
    let bks' = List.map2 (fun (label, _) bk -> (label, bk)) bks new_break_conts in
    let cks' = List.map2 (fun (label, _) ck -> (label, ck)) cks new_continue_conts in
    let rk' = fun v -> IR.CallCont (finally, [return; v]) in
    let inner =
      IR.LetCont (rethrow, [e], ek e,
        IR.LetCont (return, [v], rk v,
          IR.LetCont (finally, [c; x], translate_s finally_stmt
                                       (fun () -> IR.CallCont (c, [x]))
                                       ek bks cks rk,
            translate_s body (fun () -> IR.CallCont (finally, [rest; "_"]))
              ek' bks' cks' rk'))) in
    let outer =
      List.fold_left2 (fun body name (label, k) ->
                         IR.LetCont (name, [], k (), body))
        inner c_names cks in
    let outer' =
      List.fold_left2 (fun body name (label, k) ->
                         IR.LetCont (name, [], k (), body))
        outer b_names bks in
    IR.LetCont (rest, [], translate_s next k ek bks cks rk, outer')
                                
let translate_one stmt =
  (reset_gensym ();
   translate_s stmt  (fun () -> IR.CallCont ("return", ["null"]))
     (fun e -> IR.CallCont ("throw", [e]))
     [] [] (fun v -> IR.CallCont ("return", [v])))

let translate_fun = function
  | AST.Sync (name, args, body) ->
    IR.FunDecl (name, args, "return", "throw", translate_one body)
  | AST.Async (name, args, body) ->
    (* TODO(kmillikin): There is a bug in this translation.  It needs a
       'new Future(() {...})' somewhere. *)
    let return = gensym "return" in
    let cont = gensym "cont" in
    let c = gensym "completer" in
    let translated_body =
      translate_s body
        (fun () -> IR.CallFun ("complete", ["completer"; "null"], return))
        (fun e -> IR.CallFun ("completeError", ["completer"; e], return))
        [] []
        (fun v -> IR.CallFun ("complete", ["completer"; v], return)) in
    let future_body =
      IR.LetCont (return, ["_"], IR.CallFun ("get future", [c], "return"),
        IR.LetCont (cont, [c], translated_body,
          IR.CallFun("new Completer", [], cont))) in
    IR.FunDecl (name, args, "return", "throw", future_body)
      

let test0 =
  AST.TryFinally (AST.Return (AST.Constant 42, AST.NoStatement),
                  AST.Expression (AST.Constant 13, AST.NoStatement),
                  AST.Expression (AST.Constant 4711, AST.NoStatement))

let test1 =
  AST.TryFinally (AST.Return (AST.Constant 42, AST.NoStatement),
                  AST.Return (AST.Constant 13, AST.NoStatement),
                  AST.Expression (AST.Constant 4711, AST.NoStatement))

(* Another puzzler, from Lasse Nielsen

   l1: try {
      throw 42;
    } finally {
      l2: try {
        break l1;
      } finally {
        break l2;
      }
    }
*)

let lrn_puzzler =
  AST.Label (
    "l1",
    AST.TryFinally (
      AST.Expression (AST.Throw (AST.Constant 42), AST.NoStatement),
      AST.Label(
        "l2",
        AST.TryFinally (
          AST.Break ("l1", AST.NoStatement),
          AST.Break ("l2", AST.NoStatement),
          AST.NoStatement),
        AST.NoStatement),
      AST.NoStatement),
    AST.NoStatement)

(*
=Simple await=

f() async {
   print(“A”);
   var b = await g(“B”);
   print(b);
}
*)

let simple_await' =
  AST.Async ("f", [],
    block
      [expression_stmt (AST.Call ("print", [AST.Constant 65]));
       expression_stmt (AST.Call ("print", [AST.Await (AST.Call ("g", [AST.Constant 66]))]))])

let simple_await =
  block
    [expression_stmt (AST.Call ("print", [AST.Constant 65]));
     expression_stmt (AST.Assignment ("b", AST.Await (AST.Call ("g", [AST.Constant 66]))));
     expression_stmt (AST.Call ("print", [AST.Variable "b"]))]

(*
f() async {
   var a = await g(42);
   var b = await g(4711);
   return a+b;
}

g(n) async { return n; }
*)

let await_with_return =
  block
    [return_stmt
        (AST.Call ("add", [AST.Await (AST.Call ("g", [AST.Constant 42]));
                           AST.Await (AST.Call ("g", [AST.Constant 4711]))]))]

(*
=Await multiple returns & throw =

f() async {
 try {
    await 5;
    return 42;
  } finally {
    await 6;
    return 4711;
  }
 } 
}

g(n) async { print(n); return n; }
h(n) async { print(n); throw n; }
*)

let await_multiple_ =
  AST.Async ("f", [],
    block
      [try_finally_stmt
          (block [expression_stmt (AST.Await (AST.Constant 5));
                  return_stmt (AST.Constant 42)])
          (block [expression_stmt (AST.Await (AST.Constant 6));
                  return_stmt (AST.Constant 4711)])])
