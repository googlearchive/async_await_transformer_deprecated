f(s) {
  return new Future(() => print(s));
}

g(x, y, z) {}

test0(flag) async {
  g(print('a'), await f('b'), print('c'));
  if (flag) {
    print('true');
  } else {
    print('false');
  }
  print('after');
}

test1(x) async {
  print('before');
  if (flag) {
    print('true');
  } else {
    print('false');
  }
  g(print('a'), await f('b'), print('c'));
}

test2(x) async {
  print('before');
  if (flag) {
    g(print('a'), await f('true'), print('c'));
  } else {
    g(print('a'), await f('false'), print('c'));
  }
  print('after');
}

test3(x) async {
  print('before');
  if (flag) {
    g(print('a'), await f('true'), print('c'));
  } else {
    print('false');
  }
  print('after');
}

test4(x) async {
  print('before');
  if (flag) {
    print('true');
  } else {
    g(print('a'), await f('false'), print('c'));
  }
  print('after');
}

test5(flag) async {
  g(print('a'), await f('b'), print('c'));
  if (flag) {
    print('true');
  }
  print('after');
}

test6(x) async {
  print('before');
  if (flag) {
    print('true');
  }
  g(print('a'), await f('b'), print('c'));
}

test7(x) async {
  print('before');
  if (flag) {
    g(print('a'), await f('true'), print('c'));
  }
  print('after');
}
