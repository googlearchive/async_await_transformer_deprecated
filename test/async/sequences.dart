import 'dart:async';

import 'package:unittest/unittest.dart';

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
  expect(result, equals("abc"));
  reset();
  g(h('a'), h('b'), await f('c'));
  expect(result, equals("abc"));
  reset();
  g(h('a'), await f('b'), h('c'));
  expect(result, equals("abc"));
  reset();
  g(h('a'), await f('b'), await f('c'));
  expect(result, equals("abc"));
  reset();
  g(await f('a'), h('b'), h('c'));
  expect(result, equals("abc"));
  reset();
  g(await f('a'), h('b'), await f('c'));
  expect(result, equals("abc"));
  reset();
  g(await f('a'), await f('b'), h('c'));
  expect(result, equals("abc"));
  reset();
  g(await f('a'), await f('b'), await f('c'));
  expect(result, equals("abc"));
}
