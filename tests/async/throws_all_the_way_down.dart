import 'dart:async';

main() async {
  var a = f();
  try {
    await a;
  } catch (e) {
    print(e);
  }
}

f() async {
  try {
    await g('A');
  } catch (e) {
    await g('B');
  } finally {
    await g('C');
  }
}

g(n) async {
  print(n);
  throw n;
}
