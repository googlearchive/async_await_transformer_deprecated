import 'dart:async';

import 'package:unittest/unittest.dart';

var result = "";

f(n) async => n;

g(x, y, z) {
  result += "${x}${y}${z}";
}

main() async {
  var x = 0;
  g(x, x = 1, await f(2));
  expect(result, equals("012"));
}
