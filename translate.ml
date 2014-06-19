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
type statement_cont = string -> environment -> Ir.expression
type expression_cont = string -> environment -> Ir.expression

(* These are functions, but they all have a simple first-order
   representation.  They only close over strings so they can be
   represented by tagged strings or tagged pairs of strings. *)
type exception_cont = string -> string -> environment -> Ir.expression
type finally_cont = string -> string -> string -> environment -> Ir.expression
type jump_cont = environment -> Ir.expression

type translation_mode = Sync | SyncStar | Async | AsyncStar
let mode = ref Sync
let is_sync_star () =
  match !mode with
    | SyncStar -> true
    | _ -> false

let rec translate_e (expr: Ast.expression) (env: environment) (ek: exception_cont)
    (rk: string) (k: expression_cont): Ir.expression =
  match expr with
    | Ast.Constant n ->
      let v = gensym "v" in
      Ir.LetVal (v, Ir.Constant n, k v env)
    | Ast.Variable id ->
      k (List.assoc id env) env
    | Ast.Assignment (id, expr) ->
      translate_e expr env ek rk (fun v env -> k v (update_env id v env))
    | Ast.Call (name, args) ->
      let c = gensym "cont" in
      let v = gensym "v" in
      let throw = gensym "throw" in
      let e = gensym "e" in
      translate_args args env ek rk
        (fun vs env ->
          Ir.LetCont (c, [v], k v env,
            Ir.LetCont (throw, [e], ek e rk env,
              Ir.CallFun (name, vs, c, throw))))
    | Ast.Throw expr ->
      translate_e expr env ek rk (fun v env -> ek v rk env)
    | Ast.Await expr ->
      let c = gensym "cont" in
      let x = gensym "v" in
      let throw = gensym "throw" in
      let e = gensym "e" in
      translate_e expr env ek rk
        (fun v env ->
          Ir.LetCont (c, [x], k x env,
            Ir.LetCont (throw, [e], ek e rk env,
              Ir.LetVal ("_", Ir.Await (v, c, throw),
                Ir.CallCont ("return", ["_"])))))  (* TODO(kmillikin): Wrong. *)
and translate_args (args: Ast.expression list) (env: environment) (ek: exception_cont)
    (rk: string) (k: string list -> environment -> Ir.expression): Ir.expression =
  match args with
    | [] -> k [] env
    | arg :: args ->
      translate_e arg env ek rk
        (fun v env -> translate_args args env ek rk (fun vs env -> k (v :: vs) env))

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

let mkcont3 name =
  fun v0 v1 v2 env ->
    let extra_args = List.map (fun (_, v) -> v) env in
    Ir.CallCont (name, v0 :: v1 :: v2 :: extra_args)

let fresh_env env =
  List.fold_right
    (fun (x, v) (env, parameters) ->
      let fresh = gensym x in
      ((x, fresh) :: env, fresh :: parameters))
    env
    ([], [])

let rec translate_s (stmt: Ast.statement) (env: environment) (ek: exception_cont)
    (bks: (string * jump_cont) list) (cks: (string * jump_cont) list)
    (fk: finally_cont) (rk: string) (k: statement_cont): Ir.expression =
  match stmt with
    | Ast.EmptyStatement ->
      k rk env
    | Ast.Block stmts ->
      translate_body stmts env ek bks cks fk rk k
    | Ast.Expression expr ->
      translate_e expr env ek rk (fun v env -> k rk env)
    | Ast.Return expr ->
      translate_e expr env ek rk (mkcont1 rk)
    | Ast.YieldBreak ->
      let d = gensym "done" in
      Ir.LetVal (d, Ir.Done, fk rk d rk env)
    | Ast.Yield expr ->
      let moveNext = gensym "moveNext" in
      let v = gensym "v" in
      let dispose = gensym "dispose" in
      translate_e expr env ek rk
        (fun current env ->
          Ir.LetVal (moveNext, Ir.Fun ([], "return", "throw", k "return" env),
            Ir.LetVal (dispose,
                       Ir.Fun ([], "return", "throw",
                         let d = gensym "done" in
                         Ir.LetVal (d, Ir.Done, fk "return" d "return" env)),
              Ir.LetVal (v, Ir.Single (current, moveNext, dispose),
                Ir.CallCont (rk, [v])))))
    | Ast.YieldStar expr ->
      let moveNext = gensym "moveNext" in
      let v = gensym "v" in
      let dispose = gensym "dispose" in
      translate_e expr env ek rk
        (fun current env ->
          Ir.LetVal (moveNext, Ir.Fun ([], "return", "throw", k "return" env),
            Ir.LetVal (dispose,
                       Ir.Fun ([], "return", "throw",
                         let d = gensym "done" in
                         Ir.LetVal (d, Ir.Done, fk "return" d "return" env)),
              Ir.LetVal (v, Ir.Nested (current, moveNext, dispose),
                Ir.CallCont (rk, [v])))))
    | Ast.If (expr, thn, els) ->
      let join = gensym "join" in
      let return = gensym "return" in
      let (join_env, join_parameters) = fresh_env env in
      let if_true = gensym "then" in
      let if_false = gensym "else" in
      translate_e expr env ek rk
        (fun v env ->
          Ir.LetCont (join, return :: join_parameters, k return join_env,
            Ir.LetCont (if_true, [], translate_s thn env ek bks cks fk rk (mkcont1 join),
              Ir.LetCont (if_false, [], translate_s els env ek bks cks fk rk (mkcont1 join),
                Ir.If (v, if_true, if_false)))))
    | Ast.Label (label, stmt) ->
      let break = gensym "break" in
      let return = gensym "return" in
      let (break_env, break_parameters) = fresh_env env in
      Ir.LetCont (break, return :: break_parameters, k return break_env,
        translate_s stmt env ek ((label, mkcont break) :: bks) cks fk rk (mkcont2 break "_"))
    | Ast.Break label ->
      List.assoc label bks env
    | Ast.While (label, expr, stmt) ->
      let loop = gensym "continue" in
      let loop_return = gensym "return" in
      let (loop_env, loop_parameters) = fresh_env env in
      let break = gensym "break" in
      let break_return = gensym "return" in
      let (break_env, break_parameters) = fresh_env env in
      let not_taken = gensym "not_taken" in
      let body = gensym "body" in
      Ir.LetCont (loop, loop_return :: loop_parameters,
                  translate_e expr loop_env ek loop_return
                    (fun v body_env ->
                      Ir.LetCont (break, break_return :: break_parameters, k break_return break_env,
                        Ir.LetCont (not_taken, [], (mkcont1 break rk) body_env,
                          Ir.LetCont (body, [], translate_s stmt body_env ek
                                                  ((label, mkcont break) :: bks)
                                                  ((label, mkcont loop) :: cks)
                                                  fk loop_return (mkcont1 loop),
                            Ir.If (v, body, not_taken))))),
        mkcont1 loop rk env)
    | Ast.Continue label ->
      List.assoc label cks env
    | Ast.TryCatch (body, id, catch_stmt) ->
      let rest = gensym "rest" in
      let return = gensym "return" in
      let (rest_env, rest_parameters) = fresh_env env in
      let throw = gensym "throw" in
      let e = gensym "e" in
      let throw_return = gensym "return" in
      let (throw_env, throw_parameters) = fresh_env env in
      Ir.LetCont (rest, return :: rest_parameters, k return rest_env,
        Ir.LetCont (throw, e :: throw_return :: throw_parameters,
                    translate_s catch_stmt throw_env ek bks cks fk throw_return (mkcont1 rest),
          translate_s body env (mkcont2 throw) bks cks fk rk (mkcont1 rest)))
    | Ast.TryFinally (body, finally_stmt) ->
      (* Names for all the introduced continuations and parameters. *)
      let rest = gensym "rest" in
      let (rest_env, rest_parameters) = fresh_env env in
      let rethrow = gensym "throw" in
      let e = gensym "e" in
      let rethrow_return = gensym "return" in
      let (rethrow_env, rethrow_parameters) = fresh_env env in
      let rk' = if is_sync_star() then rk else gensym "return" in
      let v = gensym "v" in
      let (return_env, return_parameters) = fresh_env env in
      let break_names = List.map (fun _ -> gensym "break") bks in
      let continue_names = List.map (fun _ -> gensym "continue") cks in
      let finally = gensym "finally" in
      let c = gensym "where" in
      let x = gensym "what" in
      let finally_return = gensym "return" in
      let (finally_env, finally_parameters) = fresh_env env in
      (* Wrap the existing continuations to go through finally. *)
      (* TODO(kmillikin): This is wrong! *)
      let rest' = fun rk env -> mkcont3 finally rest "_" rk' env in
      let ek' = mkcont3 finally rethrow in
      (* TODO(kmillikin): Break and continue needs returns as well. *)
      let bks' =
        List.map2 (fun (label, _) name -> (label, mkcont2 finally name "_")) 
          bks break_names in
      let cks' =
        List.map2 (fun (label, _) name -> (label, mkcont2 finally name "_"))
          cks continue_names in
      let fk' = mkcont3 finally in
      (* Translate the body and wrap layers and layers of continuation bindings *)
      let translated_body = translate_s body env ek' bks' cks' fk' rk' rest' in
      let inner =
        Ir.LetCont (rethrow, e :: rethrow_return :: rethrow_parameters,
                    ek e rethrow_return rethrow_env,
          Ir.LetCont (finally, c :: x :: finally_return :: finally_parameters,
                        translate_s finally_stmt finally_env ek bks cks fk finally_return
                          (mkcont2 c x),  (* TODO(kmillikin): This is probably wrong. *)
            if is_sync_star() then
              translated_body
            else
              Ir.LetCont (rk', v :: return_parameters, mkcont3 finally rk v rk return_env,
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
      Ir.LetCont (rest, "_" :: rest_parameters, k rk rest_env,  (* TODO(kmillikin): Wrong. *)
        outer)

and translate_body (stmts: Ast.statement list) (env: environment) (ek: exception_cont)
    (bks: (string * jump_cont) list) (cks: (string * jump_cont) list)
    (fk: finally_cont) (rk: string) (k: statement_cont): Ir.expression =
  match stmts with
    | [] -> k rk env
    | stmt :: stmts ->
      translate_s stmt env ek bks cks fk rk (fun rk env -> translate_body stmts env ek bks cks fk rk k)
                                
let translate_one (stmt: Ast.statement) (env: environment) (rk: string): Ir.expression =
  translate_s stmt env
    (fun e rk env -> Ir.CallCont ("throw", [e]))
    [] []
    (fun where what rk env -> Ir.CallCont(where, [what]))
    rk
    (fun rk env -> Ir.CallCont (rk, ["null"]))

let translate_fun: (Ast.function_declaration -> Ir.function_declaration) = function
  | Ast.Sync (name, parameters, locals, body) ->
    reset_gensym();
    let env = initial_env parameters locals in
    let rk = gensym "return" in
    let v = gensym "v" in
    let (return_env, return_parameters) = fresh_env env in
    let translated_body = translate_one body env rk in
    let wrapped_body =
      List.fold_right (fun local expr -> Ir.LetVal (local, Ir.Constant 0, expr))
        locals translated_body in
    Ir.FunDecl (name, parameters, "return", "throw",
      Ir.LetCont (rk, v :: return_parameters, Ir.CallCont ("return", [v]),
        wrapped_body))

  | Ast.SyncStar (name, parameters, locals, body) ->
    mode := SyncStar;
    reset_gensym();
    let f = gensym "f" in
    let translated_body =
      translate_s body (initial_env parameters locals)
        (fun e rk env -> Ir.CallCont ("throw", [e]))
        [] []
        (fun where what rk env -> Ir.CallCont(where, [what]))
        "return"
        (fun rk env ->
          let v = gensym "v" in
          Ir.LetVal (v, Ir.Done, Ir.CallCont (rk, [v]))) in
    Ir.FunDecl (name, parameters, "return", "throw",
      Ir.LetVal (f, Ir.Fun ([], "return", "throw", translated_body),
        Ir.CallFun("newNestedIterable", [f], "return", "throw")))

  | Ast.Async (name, parameters, locals, body) ->
    mode := Async;
    reset_gensym();
    let env = initial_env parameters locals in
    let rk = gensym "return" in
    let v = gensym "v" in
    let (return_env, return_parameters) = fresh_env env in
    let f = gensym "f" in
    let translated_body =
      translate_s body env
        (fun e rk env ->
          Ir.CallFun ("completeError", ["completer"; e], rk, "throw"))
        [] []
        (fun where what rk env -> Ir.CallCont (where, [what]))
        rk
        (fun rk env ->
          Ir.CallFun ("complete", ["completer"; "null"], rk, "throw")) in
    Ir.FunDecl (name, parameters, "return", "throw",
      Ir.LetCont (rk, v :: return_parameters,
                  Ir.CallFun ("complete", ["completer"; v], "return", "throw"),
        Ir.LetVal (f, Ir.Fun (["completer"], "return", "throw", translated_body),
          Ir.CallFun("new AsyncFuture", [f], "return", "throw"))));;


Ir.write_ir stdout (List.map translate_fun (Ast.read_asts stdin))
