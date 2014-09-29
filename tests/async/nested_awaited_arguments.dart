import 'dart:async';

f(s) {
  return new Future(() => print(s));
}

g(x, y, z) {}

test1() async {
  g(print('a'),
      print('b'),
      g(print('1'), await f('2'), print('3')));
}

test2() async {
  g(print('a'),
      g(print('1'), await f('2'), print('3')),
      print('b'));
}

test3() async {
  g(print('a'),
      g(print('1'), await f('2'), print('3')),
      g(print('4'), await f('5'), print('6')));
}

test4() async {
  g(g(print('1'), await f('2'), print('3')),
      print('a'),
      print('b'));
}
