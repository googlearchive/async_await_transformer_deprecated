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
  var a = new A();
  print(await a.x);
  a.x = 7;
  print(await a.doubleIt());
  await(a.setX(0));
  print(await a.x);
  print(await a.y);
}
