import 'dart:async';

f(s) {
  return new Future(() => print(s));
}

g(x, y, z) {}

test0() async {
  g(print('a'), print('b'), print('c'));
}

test1() async {
  g(print('a'), print('b'), await f('c'));
}

test2() async {
  g(print('a'), await f('b'), print('c'));
}

test3() async {
  g(print('a'), await f('b'), await f('c'));
}

test4() async {
  g(await f('a'), print('b'), print('c'));
}

test5() async {
  g(await f('a'), print('b'), await f('c'));
}

test6() async {
  g(await f('a'), await f('b'), print('c'));
}

test7() async {
  g(await f('a'), await f('b'), await f('c'));
}
  
