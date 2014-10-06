import 'dart:async';

class A {
  var x;
  var y;

  f(x) => x;
  g(x) => x;
}

id(n) async => n;

test0() async {
  var a = new A();
  a..f(0)..g(await id(1));
  a..f(await id(0))..g(1);
  a..f(await id(0))..x = 0..y = 1;
  a..f(0)..x = await id(0)..y = 1;
  a..x = 0..f(1)..y = await id(1);

  var list = [0, 0, 0];
  list..[0] = 0..[1] = 2..[2] = 4;
}
