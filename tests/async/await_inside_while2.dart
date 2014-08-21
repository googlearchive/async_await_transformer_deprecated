import 'dart:async';

f(s) {
  return new Future(() => print(s));
}

g(x, y, z) {}

test0(flag) async {
  print('1');
  while (flag) {
    g(print('2'), await f('3'), print('4'));
  }
  print('5');
}

test1(flag) async {
  print('1');
  while (await f('2')) {
    print('3');
  }
  print('4');
}

test2(flag) async {
  print('1');
  while (await f('2')) {
    g(print('3'), await f('4'), print('5'));
  }
  print('6');
}

test3(flag) async {
  print('1');
  while (flag) {
    print('2');
  }
  g(print('3'), await f('4'), print('5'));
}

test4(flag) async {
  g(print('1'), await f('2'), print('3'));
  while (flag) {
    print('4');
  }
  print('5');
}
