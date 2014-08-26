import 'dart:async';

import 'package:expect/expect.dart';

var result = "";

main() async {
  var a = f();
  Expect.isTrue(a is Future);
  var b = await a;
  Expect.equals(result, "AB");
}

f() async {
  result += "A";
  var b = await g('B');
  result += b;
}

g(n) {
   return new Future(() => n);
}
