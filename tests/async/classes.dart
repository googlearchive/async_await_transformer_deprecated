import 'dart:async';

p(x) async { print(x); }

class A {
  int _x = 42;

  Future<int> get x async => _x;
  set x(int v) { _x = v; }

  get y async { await p('getting y'); return 'y'; }

  Future<int> doubleIt() async => (await x) * 2;

  Future setX(int v) async { x = v; }
}

main() async {
  var A = new A();
  print(await A.x);
  A.x = 7;
  print(await A.doubleIt());
  await(A.setX(0));
  print(await A.x);
  print(await A.y);
}
