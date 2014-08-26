import 'dart:async';

import 'package:expect/expect.dart';

var result = "";

f(n) async => n;

g(x, y, z) {
  result += "${x}${y}${z}";
}

main() async {
  var x = 0;
  g(x, x = 1, await f(2));
  Expect.equals(result, "012");
}
