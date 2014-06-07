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
    | Ast.Constant n ->
      let v = gensym "v" in
      Ir.LetVal (v, Ir.Constant n, k v env)
    | Ast.Variable id ->
      k (List.assoc id env) env
    | Ast.Assignment (id, expr) ->
      translate_e expr env (fun v env -> k v (update_env id v env)) ek
    | Ast.Call (name, args) ->
      let c = gensym "cont" in
      let v = gensym "v" in
      let throw = gensym "throw" in
      let e = gensym "e" in
      translate_args args env
        (fun vs env ->
          Ir.LetCont (c, [v], k v env,
            Ir.LetCont (throw, [e], ek e env,
              Ir.CallFun (name, vs, c, throw))))
        ek
    | Ast.Throw expr ->
      translate_e expr env ek ek
    | Ast.Await expr ->
      let c = gensym "cont" in
      let x = gensym "v" in
      let throw = gensym "throw" in
      let e = gensym "e" in
      translate_e expr env
        (fun v env ->
          Ir.LetCont (c, [x], k x env,
            Ir.LetCont (throw, [e], ek e env,
              Ir.LetVal ("_", Ir.Await (v, c, throw),
                Ir.CallCont ("return", ["_"])))))
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
    Ir.CallCont (name, args)

let mkcont1 name =
  fun v env ->
    let extra_args = List.map (fun (_, v) -> v) env in
    Ir.CallCont (name, v :: extra_args)

let mkcont2 name =
  fun v0 v1 env ->
    let extra_args = List.map (fun (_, v) -> v) env in
    Ir.CallCont (name, v0 :: v1 :: extra_args)

let fresh_env env =
  List.fold_right
    (fun (x, v) (env, parameters) ->
      let fresh = gensym x in
      ((x, fresh) :: env, fresh :: parameters))
    env
    ([], [])

let rec translate_s stmt env k ek bks cks rk =
  match stmt with
    | Ast.NoStatement ->
      k env
    | Ast.Expression (expr, next) ->
      translate_e expr env
        (fun v env -> translate_s next env k ek bks cks rk) ek
    | Ast.Return (expr, next) ->
      translate_e expr env rk ek
    | Ast.YieldBreak next ->
      let v = gensym "v" in
      Ir.LetVal (v, Ir.Done, rk v env)
    | Ast.Yield (expr, next) ->
      let moveNext = gensym "moveNext" in
      let v = gensym "v" in
      translate_e expr env
        (fun current env ->
          Ir.LetVal (moveNext, Ir.Fun ([], "return", "throw",
                                       translate_s next env k ek bks cks rk),
            Ir.LetVal (v, Ir.Single (current, moveNext),
              rk v env))) ek
    | Ast.YieldStar (expr, next) ->
      let moveNext = gensym "moveNext" in
      let v = gensym "v" in
      translate_e expr env
        (fun current env ->
          Ir.LetVal (moveNext, Ir.Fun ([], "return", "throw",
                                       translate_s next env k ek bks cks rk),
            Ir.LetVal (v, Ir.Nested (current, moveNext),
              rk v env))) ek
    | Ast.If (expr, thn, els, next) ->
      let join = gensym "join" in
      let (join_env, join_parameters) = fresh_env env in
      let if_true = gensym "then" in
      let if_false = gensym "else" in
      translate_e expr env
        (fun v env ->
	  Ir.LetCont (join, join_parameters, translate_s next join_env k ek bks cks rk,
            Ir.LetCont (if_true, [], translate_s thn env (mkcont join) ek bks cks rk,
              Ir.LetCont (if_false, [], translate_s els env (mkcont join) ek bks cks rk,
                Ir.If (v, if_true, if_false)))))
        ek
    | Ast.Label (label, stmt, next) ->
      let break = gensym "break" in
      let (break_env, break_parameters) = fresh_env env in
      Ir.LetCont (break, "_" :: break_parameters, translate_s next break_env k ek bks cks rk,
        translate_s stmt env (mkcont1 break "_") ek ((label, mkcont1 break) :: bks) cks rk)
    | Ast.Break (label, next) ->
      List.assoc label bks "_" env
    | Ast.While (label, expr, stmt, next) ->
      let loop = gensym "continue" in
      let (loop_env, loop_parameters) = fresh_env env in
      let break = gensym "break" in
      let (break_env, break_parameters) = fresh_env env in
      let not_taken = gensym "not_taken" in
      let body = gensym "body" in
      Ir.LetCont (loop, "_" :: loop_parameters,
                  translate_e expr loop_env
                    (fun v body_env ->
                      Ir.LetCont (break, "_" :: break_parameters,
                                  translate_s next break_env k ek bks cks rk,
                        Ir.LetCont (not_taken, [], (mkcont break) body_env,
                          Ir.LetCont (body, [], translate_s stmt body_env
                                                    (mkcont1 loop "_") ek
                                                    ((label, mkcont1 break) :: bks)
                                                    ((label, mkcont1 loop) :: cks)
                                                    rk,
                            Ir.If (v, body, not_taken))))) ek,
       (mkcont1 loop "_") env)
    | Ast.Continue (label, next) ->
      List.assoc label cks "_" env
    | Ast.TryCatch (body, id, catch_stmt, next) ->
      let rest = gensym "rest" in
      let (rest_env, rest_parameters) = fresh_env env in
      let throw = gensym "throw" in
      let e = gensym "e" in
      let (throw_env, throw_parameters) = fresh_env env in
      Ir.LetCont (rest, rest_parameters, translate_s next rest_env k ek bks cks rk,
        Ir.LetCont (throw, e :: throw_parameters,
                    translate_s catch_stmt throw_env (mkcont rest) ek bks cks rk,
          translate_s body env (mkcont rest) (mkcont1 throw) bks cks rk))
    | Ast.TryFinally (body, finally_stmt, next) ->
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
      Ir.LetCont (rethrow, e :: rethrow_parameters, ek e rethrow_env,
        Ir.LetCont (return, v :: return_parameters, rk v return_env,
          Ir.LetCont (finally, c :: x :: finally_parameters,
                      translate_s finally_stmt finally_env
                        (mkcont1 c x) ek bks cks rk,
            translated_body))) in
    let middle =
      List.fold_left2
        (fun body name (_, ck) ->
          let (env, parameters) = fresh_env env in
          Ir.LetCont (name, "_" :: parameters, ck "_" env, body))
        inner continue_names cks in
    let outer =
      List.fold_left2
        (fun body name (_, bk) ->
          let (env, parameters) = fresh_env env in
          Ir.LetCont (name, "_" :: parameters, bk "_" env, body))
        middle break_names bks in
    Ir.LetCont (rest, rest_parameters, translate_s next rest_env k ek bks cks rk,
      outer)
                                
let translate_one stmt env =
  translate_s stmt env
    (fun env -> Ir.CallCont ("return", ["null"]))
    (fun e env -> Ir.CallCont ("throw", [e]))
    [] [] (fun v env -> Ir.CallCont ("return", [v]))

let translate_fun = function
  | Ast.Sync (name, parameters, locals, body) ->
    let _ = reset_gensym () in
    let translated_body = translate_one body (initial_env parameters locals) in
    let wrapped_body =
      List.fold_right (fun local expr -> Ir.LetVal (local, Ir.Constant 0, expr))
        locals translated_body in
    Ir.FunDecl (name, parameters, "return", "throw", wrapped_body)

  | Ast.SyncStar (name, parameters, locals, body) ->
    let _ = reset_gensym () in
    let f = gensym "f" in
    let translated_body =
      translate_s body (initial_env parameters locals)
        (fun env ->
          let v = gensym "v" in Ir.LetVal (v, Ir.Done, Ir.CallCont ("return", [v])))
        (fun e env -> Ir.CallCont ("throw", [e]))
        [] []
        (fun v env -> Ir.CallCont ("return", [v])) in
    Ir.FunDecl (name, parameters, "return", "throw",
      Ir.LetVal (f, Ir.Fun ([], "return", "throw", translated_body),
        Ir.CallFun("new NestedIterable", [f], "return", "throw")))

  | Ast.Async (name, parameters, locals, body) ->
    let _ = reset_gensym () in
    let f = gensym "f" in
    let translated_body =
      translate_s body (initial_env parameters locals)
        (fun env ->
          Ir.CallFun ("complete", ["completer"; "null"], "return", "throw"))
        (fun e env ->
          Ir.CallFun ("completeError", ["completer"; e], "return", "throw"))
        [] []
        (fun v env ->
          Ir.CallFun ("complete", ["completer"; v], "return", "throw")) in
    Ir.FunDecl (name, parameters, "return", "throw",
      Ir.LetVal (f, Ir.Fun (["completer"], "return", "throw", translated_body),
        Ir.CallFun("new AsyncFuture", [f], "return", "throw")));;


Ir.write_ir stdout (List.map translate_fun (Ast.read_asts stdin))
