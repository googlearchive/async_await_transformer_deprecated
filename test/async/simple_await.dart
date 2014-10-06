import 'dart:async';

import 'package:unittest/unittest.dart';

var result = "";

main() async {
  var a = f();
  expect(a is Future, isTrue);
  var b = await a;
  expect(result, equals("AB"));
}

f() async {
  result += "A";
  var b = await g('B');
  result += b;
}

g(n) {
  return new Future(() => n);
}
