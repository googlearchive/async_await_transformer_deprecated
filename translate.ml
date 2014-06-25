(* Name generation. *)
let gensym_counter = ref 0

let reset_gensym () = gensym_counter := 0

let gensym (base_name: string): string =
  let id = base_name ^ string_of_int (!gensym_counter) in
  (gensym_counter := !gensym_counter + 1; id)


(* The compile-time environment maps local variable names to names in the
   intermediate representation. *)
type environment = (string * string) list

(* Initially map every name to itself. *)
let initial_env parameters locals =
  List.map (fun x -> (x, x)) (parameters @ locals)

(* Add a new binding. *)
let update_env (x: string) (v: string) (env: environment): environment =
  List.map (fun (x', v') -> if x' = x then (x, v) else (x', v')) env

(* An environment with the same names but bound to fresh identifiers.
   Also includes the list of fresh identifiers and fresh return and
   throw identifiers as a convenience. *)
let fresh_env env =
  List.fold_right
    (fun (x, v) (env, parameters, return, throw) ->
      let fresh = gensym x in
      ((x, fresh) :: env, fresh :: parameters, return, throw))
    env
    ([], [], gensym "return", gensym "throw")

(* The translation itself is written in CPS. *)
type statement_cont = string -> string -> environment -> Ir.expression
type expression_cont = string -> string -> string -> environment -> Ir.expression
type expression_list_cont =
    string list -> string -> string -> environment -> Ir.expression

let mkcont name =
  fun return throw (env: environment) ->
    let args = List.map (fun (_, v) -> v) env in
    Ir.CallCont (name, return :: throw :: args)

let mkcont1 name =
  fun v return throw (env: environment) ->
    let extra_args = List.map (fun (_, v) -> v) env in
    Ir.CallCont (name, v :: return :: throw :: extra_args)

let mkcont2 name =
  fun v0 v1 return throw (env: environment) ->
    let extra_args = List.map (fun (_, v) -> v) env in
    Ir.CallCont (name, v0 :: v1 :: return :: throw :: extra_args)


let rec translate_e (expr: Ast.expression) (return: string) (throw: string)
    (env: environment)
    (ek: expression_cont)
    (k: expression_cont): Ir.expression =
  match expr with
    | Ast.Constant n ->
      let v = gensym "v" in
      Ir.LetVal (v, Ir.Constant n, k v return throw env)
    | Ast.Variable id ->
      k (List.assoc id env) return throw env
    | Ast.Assignment (id, expr) ->
      translate_e expr return throw env ek
        (fun v return throw env ->
          k v return throw (update_env id v env))
    | Ast.Call (name, args) ->
      translate_args args return throw env ek
        (fun vs return throw env ->
          let cont = gensym "cont" in
          let v = gensym "v" in
          let error = gensym "error" in
          let e = gensym "e" in
          Ir.LetCont (cont, [v], k v return throw env,
            Ir.LetCont (error, [e], ek e return throw env,
              Ir.CallFun (name, vs, cont, error))))
    | Ast.Throw expr ->
      translate_e expr return throw env ek ek
    | Ast.Await expr ->
      translate_e expr return throw env ek
        (fun v return throw env ->
          let cont = gensym "cont" in
          let v = gensym "v" in
          let cont_return = gensym "return" in
          let cont_throw = gensym "throw" in
          let error = gensym "error" in
          let e = gensym "e" in
          let error_return = gensym "return" in
          let error_throw = gensym "throw" in
          let future = gensym "future" in
          Ir.LetVal (cont,
                     Ir.Fun ([v], cont_return, cont_throw,
                             k v cont_return cont_throw env),
            Ir.LetVal (error,
                       Ir.Fun ([e], error_return, error_throw,
                               ek e error_return error_throw env),
              Ir.LetVal (future, Ir.Await (v, cont, error),
                Ir.CallCont (return, [future])))))
and translate_args (args: Ast.expression list) (return: string) (throw: string)
    (env: environment)
    (ek: expression_cont)
    (k: expression_list_cont): Ir.expression =
  match args with
    | [] -> k [] return throw env
    | arg :: args ->
      translate_e arg return throw env ek
        (fun v return throw env ->
          translate_args args return throw env ek
            (fun vs return throw env ->
              k (v :: vs) return throw env))


let rec translate_s (stmt: Ast.statement) (return: string) (throw: string)
    (env: environment)
    (rk: expression_cont)
    (bks: (string * statement_cont) list)
    (cks: (string * statement_cont) list)
    (ek: expression_cont)
    (k: statement_cont): Ir.expression =
  match stmt with
    | Ast.EmptyStatement ->
      k return throw env
    | Ast.Block stmts ->
      translate_body stmts return throw env rk bks cks ek k
    | Ast.Expression expr ->
      translate_e expr return throw env ek
        (fun v return throw env ->
          k return throw env)
    | Ast.Return expr ->
      translate_e expr return throw env ek rk
    | Ast.If (expr, thn, els) ->
      translate_e expr return throw env ek
        (fun v return throw env ->
          let join = gensym "join" in
          let (join_env, join_parameters, join_return, join_throw) = fresh_env env in
          Ir.LetCont (join, join_return :: join_throw :: join_parameters,
                      k join_return join_throw join_env,
            Ir.If (v,
                   translate_s thn return throw env rk bks cks ek (mkcont join),
                   translate_s els return throw env rk bks cks ek (mkcont join))))
    | Ast.Label (label, stmt) ->
      let break = gensym "break" in
      let (break_env, break_parameters, break_return, break_throw) = fresh_env env in
      Ir.LetCont (break, break_return :: break_throw :: break_parameters,
                  k break_return break_throw break_env,
        translate_s stmt return throw env rk ((label, mkcont break) :: bks)
          cks ek (mkcont break))
    | Ast.Break label ->
      List.assoc label bks return throw env
    | Ast.While (label, expr, stmt) ->
      let loop = gensym "loop" in
      let (loop_env, loop_parameters, loop_return, loop_throw) = fresh_env env in
      Ir.LetCont (loop, loop_return :: loop_throw :: loop_parameters,
                  translate_e expr loop_return loop_throw loop_env ek
                    (fun v return throw body_env ->
                      let break = gensym "break" in
                      let (break_env, break_parameters, break_return, break_throw) =
                        fresh_env env in
                      Ir.LetCont (break, break_return :: break_throw :: break_parameters,
                                  k break_return break_throw break_env,
                        Ir.If (v,
                               translate_s stmt return throw body_env rk
                                 ((label, mkcont break) :: bks)
                                 ((label, mkcont loop) :: cks)
                                 ek
                                 (mkcont loop),
                               mkcont break return throw body_env))),
        mkcont loop return throw env)
    | Ast.Continue label ->
      List.assoc label cks return throw env
    | Ast.TryCatch (body, id, catch_stmt) ->
      let join = gensym "join" in
      let (join_env, join_parameters, join_return, join_throw) = fresh_env env in
      let catch = gensym "catch" in
      let e = gensym id in
      let (catch_env, catch_parameters, catch_return, catch_throw) = fresh_env env in
      Ir.LetCont (join, join_return :: join_throw :: join_parameters,
                  k join_return join_throw join_env,
        Ir.LetCont (catch, e :: catch_return :: catch_throw :: catch_parameters,
                    translate_s catch_stmt catch_return catch_throw catch_env
                      rk bks cks ek (mkcont join),
          translate_s body return throw env rk bks cks (mkcont1 catch) (mkcont join)))
    | Ast.TryFinally (body, finally_stmt) ->
      let join = gensym "join" in
      let (join_env, join_parameters, join_return, join_throw) = fresh_env env in
      let finally = gensym "finally" in
      let where = gensym "where" in
      let what = gensym "what" in
      let (finally_env, finally_parameters, finally_return, finally_throw) =
        fresh_env env in
      let return = gensym "return" in
      let v = gensym "v" in
      let (return_env, return_parameters, return_return, return_throw) =
        fresh_env env in
      let catch = gensym "catch" in
      let e = gensym "e" in
      let (catch_env, catch_parameters, catch_return, catch_throw) = fresh_env env in
      let breaks = List.map (fun _ -> gensym "break") bks in
      let continues = List.map (fun _ -> gensym "continue") cks in
      Ir.LetCont (join, "_" :: join_return :: join_throw :: join_parameters,
                  k join_return join_throw join_env,
        Ir.LetCont (return, v :: return_return :: return_throw :: return_parameters,
                    rk v return_return return_throw return_env,
          List.fold_right2
            (fun name (_, bk) body ->
              let (env, parameters, return, throw) = fresh_env env in
              Ir.LetCont (name, "_" :: return :: throw :: parameters,
                          bk return throw env,
                body))
            breaks
            bks
            (List.fold_right2
               (fun name (_, ck) body ->
                 let (env, parameters, return, throw) = fresh_env env in
                 Ir.LetCont (name, "_" :: return :: throw :: parameters,
                             ck return throw env,
                   body))
               continues
               cks
               (Ir.LetCont (catch,
                            e :: catch_return :: catch_throw :: catch_parameters,
                            ek e catch_return catch_throw catch_env,
                  Ir.LetCont (finally,
                              where :: what :: finally_return :: finally_throw
                                :: finally_parameters,
                              translate_s finally_stmt finally_return finally_throw
                                finally_env rk bks cks ek
                                (mkcont1 where what),
                    translate_s body return throw env (mkcont2 finally return) bks
                      (List.map2
                         (fun (label, _) continue ->
                           (label, mkcont2 finally continue "_"))
                         cks continues)
                      (mkcont2 finally catch)
                      (mkcont2 finally "join" "_")))))))
and translate_body (body: Ast.statement list) (return: string) (throw: string)
    (env: environment)
    (rk: expression_cont)
    (bks: (string * statement_cont) list)
    (cks: (string * statement_cont) list)
    (ek: expression_cont)
    (k: statement_cont): Ir.expression =
  match body with
    | [] -> k return throw env
    | stmt :: stmts ->
      translate_s stmt return throw env rk bks cks ek
        (fun return throw env ->
          translate_body stmts return throw env rk bks cks ek k)
                                
(* let translate_one (stmt: Ast.statement) (env: environment) (rk: string): Ir.expression = *)
(*   translate_s stmt env *)
(*     (fun e rk env -> Ir.CallCont ("throw", [e])) *)
(*     [] [] *)
(*     (fun where what rk env -> Ir.CallCont(where, [what])) *)
(*     rk *)
(*     (fun rk env -> Ir.CallCont (rk, ["null"])) *)

let translate_fun: (Ast.function_declaration -> Ir.function_declaration) = function
  | Ast.Sync (name, parameters, locals, body) ->
    reset_gensym ();
    let translated_body =
      translate_s body "return" "throw" (initial_env parameters locals)
        (fun v return throw env -> Ir.CallCont (return, [v]))
        []
        []
        (fun e return throw env -> Ir.CallCont (throw, [e]))
        (fun return throw env ->
          let v = gensym "v" in
          Ir.LetVal (v, Ir.Constant 0, Ir.CallCont (return, [v]))) in
    Ir.FunDecl (name, parameters, "return", "throw",
      List.fold_right
        (fun local body -> Ir.LetVal (local, Ir.Constant 0, body))
        locals
        translated_body)

  | Ast.Async (name, parameters, locals, body) ->
    reset_gensym ();
    let return = gensym "return" in
    let throw = gensym "throw" in
    let f = gensym "f" in
    let completer = gensym "completer" in
    let translated_body =
      translate_s body return throw (initial_env parameters locals)
        (fun v return throw env ->
          Ir.CallFun ("complete", [completer; v], return, throw))
        []
        []
        (fun e return throw env ->
          Ir.CallFun ("completeError", [completer; e], return, throw))
        (fun return throw env ->
          let v = gensym "v" in
          Ir.LetVal (v, Ir.Constant 0,
            Ir.CallFun ("complete", [completer; v], return, throw))) in
    Ir.FunDecl (name, parameters, "return", "throw",
      Ir.LetVal (f, Ir.Fun (["completer"], return, throw, translated_body),
        Ir.CallFun("newAsyncFuture", [f], "return", "throw")))
;;

Ir.write_ir stdout (List.map translate_fun (Ast.read_asts stdin))
