open AST
let test0 =
  TryFinally (Return (Constant 42, NoStatement),
              Expression (Constant 13, NoStatement),
              Expression (Constant 4711, NoStatement))

let test1 =
  TryFinally (Return (Constant 42, NoStatement),
              Return (Constant 13, NoStatement),
              Expression (Constant 4711, NoStatement))

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
  Label (
    "l1",
    TryFinally (
      Expression (Throw (Constant 42), NoStatement),
      Label(
        "l2",
        TryFinally (
          Break ("l1", NoStatement),
          Break ("l2", NoStatement),
          NoStatement),
        NoStatement),
      NoStatement),
    NoStatement)

(*
=Simple await=

f() async {
   print(“A”);
   var b = await g(“B”);
   print(b);
}
*)

let simple_await' =
  Async ("f", [], [],
    block
      [expression_stmt (Call ("print", [Constant 65]));
       expression_stmt (Call ("print", [Await (Call ("g", [Constant 66]))]))])

let simple_await =
  Async ("f", [], ["b"],
    block
      [expression_stmt (Call ("print", [Constant 65]));
       expression_stmt (Assignment ("b", Await (Call ("g", [Constant 66]))));
       expression_stmt (Call ("print", [Variable "b"]))])

(*
f() async {
   var a = await g(42);
   var b = await g(4711);
   return a+b;
}

g(n) async { return n; }
*)

let await_with_return =
  Async ("f", [], [],
    block
      [return_stmt
          (Call ("add", [Await (Call ("g", [Constant 42]));
                         Await (Call ("g", [Constant 4711]))]))])

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
  Async ("f", [], [],
    block
      [try_finally_stmt
          (block [expression_stmt (Await (Constant 5));
                  return_stmt (Constant 42)])
          (block [expression_stmt (Await (Constant 6));
                  return_stmt (Constant 4711)])])

(* Continuation parameters on branching control flow. *)
let branching0 =
  Sync ("f", [], ["x"],
    block
      [if_stmt (Variable "x")
          (block [expression_stmt (Assignment ("x", Constant 0))])
          (block [expression_stmt (Assignment ("x", Constant 1))]);
       return_stmt (Variable "x")])

let branching1 =
  Sync ("f", [], ["x"],
    block
      [if_stmt (Variable "x")
          (block [expression_stmt (Assignment ("x", Constant 0))])
          (block []);
       return_stmt (Variable "x")])

let branching2 =
  Sync ("f", [], ["x"],
    block
      [if_stmt (Variable "x")
          (block [])
          (block [expression_stmt (Assignment ("x", Constant 1))]);
       return_stmt (Variable "x")])

let branching3 =
  Sync ("f", [], ["x"; "y"],
   block
     [if_stmt (Variable "x")
         (block [expression_stmt (Assignment ("x", Constant 0));
                 expression_stmt (Assignment ("y", Constant 1))])
         (block [expression_stmt (Assignment ("y", Constant 0));
                 expression_stmt (Assignment ("x", Constant 1))]);
      return_stmt (Variable "x")])

let looping0 =
  Sync ("f", [], ["x"; "y"],
    block
      [while_stmt "label" (Variable "x")
          (block [expression_stmt (Assignment ("x", Constant 0));
                  expression_stmt (Assignment ("y", Constant 1))]);
       return_stmt (Variable "x")])

let looping1 =
  Sync ("f", [], ["x"; "y"],
    block
      [while_stmt "label" (Variable "x")
          (block [expression_stmt (Assignment ("x", Constant 0));
                  expression_stmt (Assignment ("y", Constant 1));
                  continue_stmt "label"]);
       return_stmt (Variable "x")])

let looping2 =
  Sync ("f", [], ["x"; "y"],
    block
      [while_stmt "label" (Variable "x")
          (block [expression_stmt (Assignment ("x", Constant 0));
                  expression_stmt (Assignment ("y", Constant 1));
                  break_stmt "label"]);
       return_stmt (Variable "x")])

(*
 range(s,c) sync*{
   if(c <= 0) return;
   yield s;
   yield* range(s+1,c-1);
 }
*)
let yield =
  SyncStar ("range", ["s"; "c"], [],
    block
      [if_stmt (Variable "c")
          (block [yield_stmt (Variable "s");
                  yield_star_stmt (Call ("range",
                                         [Variable "s"; Variable "c"]))])
          (block [yield_break_stmt])])
       
