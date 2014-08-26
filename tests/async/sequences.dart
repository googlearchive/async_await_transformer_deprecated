import 'dart:async';

import 'package:expect/expect.dart';

var result;

reset() {
  result = "";
}

f(s) {
  return new Future(() {
    result += s;
  });
}

h(s) {
  result += s;
}

g(x, y, z) {}

main() async {
  reset();
  g(h('a'), h('b'), h('c'));
  Expect.equals(result, "abc");
  reset();
  g(h('a'), h('b'), await f('c'));
  Expect.equals(result, "abc");
  reset();
  g(h('a'), await f('b'), h('c'));
  Expect.equals(result, "abc");
  reset();
  g(h('a'), await f('b'), await f('c'));
  Expect.equals(result, "abc");
  reset();
  g(await f('a'), h('b'), h('c'));
  Expect.equals(result, "abc");
  reset();
  g(await f('a'), h('b'), await f('c'));
  Expect.equals(result, "abc");
  reset();
  g(await f('a'), await f('b'), h('c'));
  Expect.equals(result, "abc");
  reset();
  g(await f('a'), await f('b'), await f('c'));
  Expect.equals(result, "abc");
}
