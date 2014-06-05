module AST = Ast
module IR = Ir

let gensym_counter = ref 0

let reset_gensym () = gensym_counter := 0

let gensym base_name =
  let id = base_name ^ string_of_int (!gensym_counter) in
  (gensym_counter := !gensym_counter + 1; id)

let initial_env parameters locals =
  List.map (fun x -> (x, x)) (parameters @ locals)

let update_env x v env =
  List.map (fun (x', v') -> if x' = x then (x, v) else (x', v')) env

(* TODO(kmillikin): Throw does not work properly.  CPS functions need
   to take a pair of continuations. *)

(* TODO(kmillikin): 1. Parse Dart to something that can be read as the
   Ocaml AST.  2. Write the Ocaml IR as something that can be parsed as
   the dart2dart IR. *)
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
                                  IR.LetVal ("_", IR.Await (v, c, throw),
                                             IR.CallCont ("return", ["_"])))))
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
    | AST.YieldBreak next ->
      let v = gensym "v" in
      IR.LetVal (v, IR.Done, rk v env)
    | AST.Yield (expr, next) ->
      let moveNext = gensym "moveNext" in
      let v = gensym "v" in
      translate_e expr env
        (fun current env ->
          IR.LetVal (moveNext, IR.Fun ([], "return",
                                       translate_s next env k ek bks cks rk),
                     IR.LetVal (v, IR.Single (current, moveNext),
                                rk v env))) ek
    | AST.YieldStar (expr, next) ->
      let moveNext = gensym "moveNext" in
      let v = gensym "v" in
      translate_e expr env
        (fun current env ->
          IR.LetVal (moveNext, IR.Fun ([], "return",
                                       translate_s next env k ek bks cks rk),
                     IR.LetVal (v, IR.Nested (current, moveNext),
                                rk v env))) ek
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
      IR.LetCont (loop, "_" :: loop_parameters,
                  translate_e expr loop_env
                    (fun v body_env ->
                      IR.LetCont (break, "_" :: break_parameters,
                                  translate_s next break_env k ek bks cks rk,
                                  IR.LetCont (not_taken, [], (mkcont break) body_env,
                                              IR.LetCont (body, [], translate_s stmt body_env
                                                (mkcont1 loop "_") ek
                                                ((label, mkcont1 break) :: bks)
                                                ((label, mkcont1 loop) :: cks)
                                                rk,
                                                          IR.If (v, body, not_taken))))) ek,
                  (mkcont1 loop "_") env)
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

  | AST.SyncStar (name, parameters, locals, body) ->
    let _ = reset_gensym () in
    let f = gensym "f" in
    let translated_body =
      translate_s body (initial_env parameters locals)
        (fun env ->
          let v = gensym "v" in IR.LetVal (v, IR.Done, IR.CallCont ("return", [v])))
        (fun e env -> IR.CallCont ("throw", [e]))
        [] []
        (fun v env -> IR.CallCont ("return", [v])) in
    IR.FunDecl (name, parameters, "return", "throw",
      IR.LetVal (f, IR.Fun ([], "return", translated_body),
        IR.CallFun("new NestedIterable", [f], "return")))

  | AST.Async (name, parameters, locals, body) ->
    let _ = reset_gensym () in
    let f = gensym "f" in
    let translated_body =
      translate_s body (initial_env parameters locals)
        (fun env ->
          IR.CallFun ("complete", ["completer"; "null"], "return"))
        (fun e env -> IR.CallFun ("completeError", ["completer"; e], "return"))
        [] []
        (fun v env -> IR.CallFun ("complete", ["completer"; v], "return")) in
    IR.FunDecl (name, parameters, "return", "throw",
      IR.LetVal (f, IR.Fun (["completer"], "return", translated_body),
        IR.CallFun("new AsyncFuture", [f], "return")))
