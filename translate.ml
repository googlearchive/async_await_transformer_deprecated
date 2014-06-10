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


(* The translation itself is written in CPS. *)
type statement_cont = environment -> Ir.expression
type expression_cont = string -> environment -> Ir.expression
type exception_cont = string -> environment -> Ir.expression
type finally_cont = string -> string -> environment -> Ir.expression
type jump_cont = environment -> Ir.expression
type return_cont = string -> environment -> Ir.expression

let rec translate_e (expr: Ast.expression) (env: environment)
    (k: expression_cont)
    (ek: exception_cont): Ir.expression =
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
and translate_args (args: Ast.expression list) (env: environment)
    (k: string list -> environment -> Ir.expression)
    (ek: exception_cont): Ir.expression =
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

let rec translate_s (stmt: Ast.statement) (env: environment)
    (k: statement_cont)
    (ek: exception_cont)
    (bks: (string * jump_cont) list)
    (cks: (string * jump_cont) list)
    (fk: finally_cont)
    (rk: return_cont): Ir.expression =
  match stmt with
    | Ast.NoStatement ->
      k env
    | Ast.Expression (expr, next) ->
      translate_e expr env
        (fun v env -> translate_s next env k ek bks cks fk rk) ek
    | Ast.Return (expr, next) ->
      translate_e expr env rk ek
    | Ast.YieldBreak next ->
      let d = gensym "done" in
      Ir.LetVal (d, Ir.Done, fk "return" d env)
    | Ast.Yield (expr, next) ->
      let moveNext = gensym "moveNext" in
      let v = gensym "v" in
      let dispose = gensym "dispose" in
      translate_e expr env
        (fun current env ->
          Ir.LetVal (moveNext, Ir.Fun ([], "return", "throw",
                                       translate_s next env k ek bks cks fk rk),
            Ir.LetVal (dispose, Ir.Fun ([], "return", "throw",
                                        let d = gensym "done" in
                                        Ir.LetVal (d, Ir.Done,
                                          fk "return" d env)),
              Ir.LetVal (v, Ir.Single (current, moveNext, dispose),
                Ir.CallCont ("return", [v]))))) ek
    | Ast.YieldStar (expr, next) ->
      let moveNext = gensym "moveNext" in
      let v = gensym "v" in
      let dispose = gensym "dispose" in
      translate_e expr env
        (fun current env ->
          Ir.LetVal (moveNext, Ir.Fun ([], "return", "throw",
                                       translate_s next env k ek bks cks fk rk),
            Ir.LetVal (dispose, Ir.Fun ([], "return", "throw",
                                        let d = gensym "done" in
                                        Ir.LetVal (d, Ir.Done,
                                          fk "return" d env)),
              Ir.LetVal (v, Ir.Nested (current, moveNext, dispose),
                Ir.CallCont ("return", [v]))))) ek
    | Ast.If (expr, thn, els, next) ->
      let join = gensym "join" in
      let (join_env, join_parameters) = fresh_env env in
      let if_true = gensym "then" in
      let if_false = gensym "else" in
      translate_e expr env
        (fun v env ->
	  Ir.LetCont (join, join_parameters, translate_s next join_env k ek bks cks fk rk,
            Ir.LetCont (if_true, [], translate_s thn env (mkcont join) ek bks cks fk rk,
              Ir.LetCont (if_false, [], translate_s els env (mkcont join) ek bks cks fk rk,
                Ir.If (v, if_true, if_false)))))
        ek
    | Ast.Label (label, stmt, next) ->
      let break = gensym "break" in
      let (break_env, break_parameters) = fresh_env env in
      Ir.LetCont (break, break_parameters, translate_s next break_env k ek bks cks fk rk,
                  translate_s stmt env (mkcont1 break "_") ek ((label, mkcont break) :: bks) cks fk rk)
    | Ast.Break (label, next) ->
      List.assoc label bks env
    | Ast.While (label, expr, stmt, next) ->
      let loop = gensym "continue" in
      let (loop_env, loop_parameters) = fresh_env env in
      let break = gensym "break" in
      let (break_env, break_parameters) = fresh_env env in
      let not_taken = gensym "not_taken" in
      let body = gensym "body" in
      Ir.LetCont (loop, loop_parameters,
                  translate_e expr loop_env
                    (fun v body_env ->
                      Ir.LetCont (break, break_parameters,
                                  translate_s next break_env k ek bks cks fk rk,
                                  Ir.LetCont (not_taken, [], (mkcont break) body_env,
                                    Ir.LetCont (body, [], translate_s stmt body_env
                                                            (mkcont loop) ek
                                                            ((label, mkcont break) :: bks)
                                                            ((label, mkcont loop) :: cks)
                                                            fk
                                                            rk,
                                      Ir.If (v, body, not_taken))))) ek,
        (mkcont loop) env)
    | Ast.Continue (label, next) ->
      List.assoc label cks env
    | Ast.TryCatch (body, id, catch_stmt, next) ->
      let rest = gensym "rest" in
      let (rest_env, rest_parameters) = fresh_env env in
      let throw = gensym "throw" in
      let e = gensym "e" in
      let (throw_env, throw_parameters) = fresh_env env in
      Ir.LetCont (rest, rest_parameters, translate_s next rest_env k ek bks cks fk rk,
        Ir.LetCont (throw, e :: throw_parameters,
                    translate_s catch_stmt throw_env (mkcont rest) ek bks cks fk rk,
          translate_s body env (mkcont rest) (mkcont1 throw) bks cks fk rk))
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
        List.map2 (fun (label, _) name -> (label, mkcont2 finally name "_")) 
          bks break_names in
      let cks' =
        List.map2 (fun (label, _) name -> (label, mkcont2 finally name "_"))
          cks continue_names in
      let fk' = mkcont2 finally in
      let rk' = mkcont2 finally return in
      (* Translate the body and wrap layers and layers of continuation bindings *)
      let translated_body = translate_s body env (mkcont rest) ek' bks' cks' fk' rk' in
      let inner =
        Ir.LetCont (rethrow, e :: rethrow_parameters, ek e rethrow_env,
          Ir.LetCont (return, v :: return_parameters, rk v return_env,
            Ir.LetCont (finally, c :: x :: finally_parameters,
                        translate_s finally_stmt finally_env
                          (mkcont1 c x) ek bks cks fk rk,
              translated_body))) in
      let middle =
        List.fold_left2
          (fun body name (_, ck) ->
            let (env, parameters) = fresh_env env in
            Ir.LetCont (name, "_" :: parameters, ck env, body))
          inner continue_names cks in
      let outer =
        List.fold_left2
          (fun body name (_, bk) ->
            let (env, parameters) = fresh_env env in
            Ir.LetCont (name, "_" :: parameters, bk env, body))
          middle break_names bks in
      Ir.LetCont (rest, rest_parameters, translate_s next rest_env k ek bks cks fk rk,
        outer)
                                
let translate_one (stmt: Ast.statement) (env: environment): Ir.expression =
  translate_s stmt env
    (fun env -> Ir.CallCont ("return", ["null"]))
    (fun e env -> Ir.CallCont ("throw", [e]))
    [] []
    (fun what where env -> Ir.CallCont(where, [what]))
    (fun v env -> Ir.CallCont ("return", [v]))

let translate_fun: (Ast.function_declaration -> Ir.function_declaration) = function
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
        (fun what where env -> Ir.CallCont(where, [what]))
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
        (fun what where env -> Ir.CallCont (where, [what]))
        (fun v env ->
          Ir.CallFun ("complete", ["completer"; v], "return", "throw")) in
    Ir.FunDecl (name, parameters, "return", "throw",
      Ir.LetVal (f, Ir.Fun (["completer"], "return", "throw", translated_body),
        Ir.CallFun("new AsyncFuture", [f], "return", "throw")));;


Ir.write_ir stdout (List.map translate_fun (Ast.read_asts stdin))
