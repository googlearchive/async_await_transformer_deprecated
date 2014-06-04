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
    | Async of string * string list * string list * statement
end

let expression_stmt expr rest = AST.Expression (expr, rest)
let return_stmt expr rest = AST.Return (expr, rest)
let if_stmt expr thn els rest = AST.If (expr, thn, els, rest)
let label_stmt label stmt rest = AST.Label (label, stmt, rest)
let break_stmt label rest = AST.Break (label, rest)
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

let initial_env parameters locals =
  List.map (fun x -> (x, x)) (parameters @ locals)

let update_env x v env =
  List.map (fun (x', v') -> if x' = x then (x, v) else (x', v')) env

let rec translate_e expr env k ek =
  match expr with
  | AST.Constant n ->
    let v = gensym "v" in
    IR.LetVal (v, IR.Constant n, k v env)
  | AST.Variable id ->
    k (List.assoc id env) env
  | AST.Assignment (id, expr) ->
    translate_e expr env (fun v env -> k v (update_env id v env)) ek
  | AST.Call (name, args) ->
    let c = gensym "cont" in
    let v = gensym "v" in
    translate_args args env
      (fun vs env ->
        IR.LetCont (c, [v], k v env, IR.CallFun (name, vs, c)))
      ek
  | AST.Throw expr ->
    translate_e expr env ek ek
  | AST.Await expr ->
    let c = gensym "cont" in
    let x = gensym "v" in
    let throw = gensym "throw" in
    let e = gensym "e" in
    translate_e expr env
      (fun v env ->
        IR.LetCont (c, [x], k x env,
          IR.LetCont (throw, [e], ek e env,
            IR.Await (v, c, throw))))
      ek
and translate_args args env k ek =
  match args with
    | [] -> k [] env
    | arg :: args ->
      translate_e arg env
        (fun v env -> translate_args args env (fun vs env -> k (v :: vs) env) ek)
        ek

let mkcont name =
  fun env ->
    let args = List.map (fun (_, v) -> v) env in
    IR.CallCont (name, args)

let mkcont1 name =
  fun v env ->
    let extra_args = List.map (fun (_, v) -> v) env in
    IR.CallCont (name, v :: extra_args)

let mkcont2 name =
  fun v0 v1 env ->
    let extra_args = List.map (fun (_, v) -> v) env in
    IR.CallCont (name, v0 :: v1 :: extra_args)

let fresh_env env =
  List.fold_right
    (fun (x, v) (env, parameters) ->
      let fresh = gensym x in
      ((x, fresh) :: env, fresh :: parameters))
    env
    ([], [])

let rec translate_s stmt env k ek bks cks rk =
  match stmt with
  | AST.NoStatement ->
    k env
  | AST.Expression (expr, next) ->
    translate_e expr env
      (fun v env -> translate_s next env k ek bks cks rk) ek
  | AST.Return (expr, next) ->
    translate_e expr env rk ek
  | AST.If (expr, thn, els, next) ->
    let join = gensym "join" in
    let (join_env, join_parameters) = fresh_env env in
    let if_true = gensym "then" in
    let if_false = gensym "else" in
    translate_e expr env
      (fun v env ->
	IR.LetCont (join, join_parameters, translate_s next join_env k ek bks cks rk,
          IR.LetCont (if_true, [], translate_s thn env (mkcont join) ek bks cks rk,
            IR.LetCont (if_false, [], translate_s els env (mkcont join) ek bks cks rk,
              IR.If (v, if_true, if_false)))))
      ek
  | AST.Label (label, stmt, next) ->
    let break = gensym "break" in
    let (break_env, break_parameters) = fresh_env env in
    IR.LetCont (break, "_" :: break_parameters, translate_s next break_env k ek bks cks rk,
      translate_s stmt env (mkcont1 break "_") ek ((label, mkcont1 break) :: bks) cks rk)
  | AST.Break (label, next) ->
    List.assoc label bks "_" env
  | AST.While (label, expr, stmt, next) ->
    let loop = gensym "continue" in
    let (loop_env, loop_parameters) = fresh_env env in
    let break = gensym "break" in
    let (break_env, break_parameters) = fresh_env env in
    let not_taken = gensym "not_taken" in
    let body = gensym "body" in
    IR.LetCont (loop, loop_parameters,
                translate_e expr loop_env
                  (fun v body_env ->
                    IR.LetCont (break, break_parameters,
                                translate_s next break_env k ek bks cks rk,
                      IR.LetCont (not_taken, [], (mkcont break) body_env,
                        IR.LetCont (body, [], translate_s stmt body_env
                                                (mkcont1 loop "_") ek
                                                ((label, mkcont1 break) :: bks)
                                                ((label, mkcont1 loop) :: cks)
                                                rk,
                          IR.If (v, body, not_taken))))) ek,
      (mkcont loop) env)
  | AST.Continue (label, next) ->
    List.assoc label cks "_" env
  | AST.TryCatch (body, id, catch_stmt, next) ->
    let rest = gensym "rest" in
    let (rest_env, rest_parameters) = fresh_env env in
    let throw = gensym "throw" in
    let e = gensym "e" in
    let (throw_env, throw_parameters) = fresh_env env in
    IR.LetCont (rest, rest_parameters, translate_s next rest_env k ek bks cks rk,
      IR.LetCont (throw, e :: throw_parameters,
                  translate_s catch_stmt throw_env (mkcont rest) ek bks cks rk,
        translate_s body env (mkcont rest) (mkcont1 throw) bks cks rk))
  | AST.TryFinally (body, finally_stmt, next) ->
    (* Names for all the introduced continuations and parameters. *)
    let rest = gensym "rest" in
    let (rest_env, rest_parameters) = fresh_env env in
    let rethrow = gensym "throw" in
    let e = gensym "e" in
    let (rethrow_env, rethrow_parameters) = fresh_env env in
    let return = gensym "return" in
    let v = gensym "v" in
    let (return_env, return_parameters) = fresh_env env in
    let break_names = List.map (fun _ -> gensym "break") bks in
    let continue_names = List.map (fun _ -> gensym "continue") cks in
    let finally = gensym "finally" in
    let c = gensym "where" in
    let x = gensym "what" in
    let (finally_env, finally_parameters) = fresh_env env in
    (* Wrap the existing continuations to go through finally. *)
    let ek' = mkcont2 finally rethrow in
    let bks' =
      List.map2 (fun (label, _) name -> (label, mkcont2 finally name)) 
        bks break_names in
    let cks' =
      List.map2 (fun (label, _) name -> (label, mkcont2 finally name))
        cks continue_names in
    let rk' = mkcont2 finally return in
    (* Translate the body and wrap layers and layers of continuation bindings *)
    let translated_body = translate_s body env (mkcont rest) ek' bks' cks' rk' in
    let inner =
      IR.LetCont (rethrow, e :: rethrow_parameters, ek e rethrow_env,
        IR.LetCont (return, v :: return_parameters, rk v return_env,
          IR.LetCont (finally, c :: x :: finally_parameters,
                      translate_s finally_stmt finally_env
                        (mkcont1 c x) ek bks cks rk,
            translated_body))) in
    let middle =
      List.fold_left2
        (fun body name (_, ck) ->
          let (env, parameters) = fresh_env env in
          IR.LetCont (name, "_" :: parameters, ck "_" env, body))
        inner continue_names cks in
    let outer =
      List.fold_left2
        (fun body name (_, bk) ->
          let (env, parameters) = fresh_env env in
          IR.LetCont (name, "_" :: parameters, bk "_" env, body))
        middle break_names bks in
    IR.LetCont (rest, rest_parameters, translate_s next rest_env k ek bks cks rk,
      outer)
                                
let translate_one stmt env =
  translate_s stmt env
    (fun env -> IR.CallCont ("return", ["null"]))
    (fun e env -> IR.CallCont ("throw", [e]))
    [] [] (fun v env -> IR.CallCont ("return", [v]))

let translate_fun = function
  | AST.Sync (name, parameters, locals, body) ->
    let _ = reset_gensym () in
    let translated_body = translate_one body (initial_env parameters locals) in
    let wrapped_body =
      List.fold_right (fun local expr -> IR.LetVal (local, IR.Constant 0, expr))
        locals translated_body in
    IR.FunDecl (name, parameters, "return", "throw", wrapped_body)
  | AST.Async (name, parameters, locals, body) ->
    (* TODO(kmillikin): There is a bug in this translation.  It needs a
       'new Future(() {...})' somewhere. *)
    let _ = reset_gensym () in
    let return = gensym "return" in
    let cont = gensym "cont" in
    let c = gensym "completer" in
    let translated_body =
      translate_s body (initial_env parameters locals)
        (fun env -> IR.CallFun ("complete", ["completer"; "null"], return))
        (fun e env -> IR.CallFun ("completeError", ["completer"; e], return))
        [] []
        (fun v env -> IR.CallFun ("complete", ["completer"; v], return)) in
    let future_body =
      IR.LetCont (return, ["_"], IR.CallFun ("get future", [c], "return"),
        IR.LetCont (cont, [c], translated_body,
          IR.CallFun("new Completer", [], cont))) in
    IR.FunDecl (name, parameters, "return", "throw", future_body)
      

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
  AST.Async ("f", [], [],
    block
      [expression_stmt (AST.Call ("print", [AST.Constant 65]));
       expression_stmt (AST.Call ("print", [AST.Await (AST.Call ("g", [AST.Constant 66]))]))])

let simple_await =
  AST.Async ("f", [], ["b"],
    block
      [expression_stmt (AST.Call ("print", [AST.Constant 65]));
       expression_stmt (AST.Assignment ("b", AST.Await (AST.Call ("g", [AST.Constant 66]))));
       expression_stmt (AST.Call ("print", [AST.Variable "b"]))])

(*
f() async {
   var a = await g(42);
   var b = await g(4711);
   return a+b;
}

g(n) async { return n; }
*)

let await_with_return =
  AST.Async ("f", [], [],
    block
      [return_stmt
          (AST.Call ("add", [AST.Await (AST.Call ("g", [AST.Constant 42]));
                             AST.Await (AST.Call ("g", [AST.Constant 4711]))]))])

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
  AST.Async ("f", [], [],
    block
      [try_finally_stmt
          (block [expression_stmt (AST.Await (AST.Constant 5));
                  return_stmt (AST.Constant 42)])
          (block [expression_stmt (AST.Await (AST.Constant 6));
                  return_stmt (AST.Constant 4711)])])

(* Continuation parameters on branching control flow. *)
let branching0 =
  AST.Sync ("f", [], ["x"],
    block
      [if_stmt (AST.Variable "x")
          (block [expression_stmt (AST.Assignment ("x", AST.Constant 0))])
          (block [expression_stmt (AST.Assignment ("x", AST.Constant 1))]);
       return_stmt (AST.Variable "x")])

let branching1 =
  AST.Sync ("f", [], ["x"],
    block
      [if_stmt (AST.Variable "x")
          (block [expression_stmt (AST.Assignment ("x", AST.Constant 0))])
          (block []);
       return_stmt (AST.Variable "x")])

let branching2 =
  AST.Sync ("f", [], ["x"],
    block
      [if_stmt (AST.Variable "x")
          (block [])
          (block [expression_stmt (AST.Assignment ("x", AST.Constant 1))]);
       return_stmt (AST.Variable "x")])

let branching3 =
  AST.Sync ("f", [], ["x"; "y"],
   block
     [if_stmt (AST.Variable "x")
         (block [expression_stmt (AST.Assignment ("x", AST.Constant 0));
                 expression_stmt (AST.Assignment ("y", AST.Constant 1))])
         (block [expression_stmt (AST.Assignment ("y", AST.Constant 0));
                 expression_stmt (AST.Assignment ("x", AST.Constant 1))]);
      return_stmt (AST.Variable "x")])

let looping0 =
  AST.Sync ("f", [], ["x"; "y"],
    block
      [while_stmt "label" (AST.Variable "x")
          (block [expression_stmt (AST.Assignment ("x", AST.Constant 0));
                  expression_stmt (AST.Assignment ("y", AST.Constant 1))]);
       return_stmt (AST.Variable "x")])

let looping1 =
  AST.Sync ("f", [], ["x"; "y"],
    block
      [while_stmt "label" (AST.Variable "x")
          (block [expression_stmt (AST.Assignment ("x", AST.Constant 0));
                  expression_stmt (AST.Assignment ("y", AST.Constant 1));
                  continue_stmt "label"]);
       return_stmt (AST.Variable "x")])

let looping2 =
  AST.Sync ("f", [], ["x"; "y"],
    block
      [while_stmt "label" (AST.Variable "x")
          (block [expression_stmt (AST.Assignment ("x", AST.Constant 0));
                  expression_stmt (AST.Assignment ("y", AST.Constant 1));
                  break_stmt "label"]);
       return_stmt (AST.Variable "x")])
