import 'dart:async';

class A {
  var x;
  A(this.x);
}

f() async => 42;
g() async => true;
h() async => new A(0);

main() async {
  print(!await g());
  print(-await f() + 1);
  print(~await f() + 1);
  print(++(await h()).x);
  print(--(await h()).x);
}
