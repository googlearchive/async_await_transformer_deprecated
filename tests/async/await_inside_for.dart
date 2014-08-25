p(x) async { print(x); }
zero() async => 0;
compare(n) async => n < 10;
inc(n) async => n + 1;

// Test await in body, initializer, condition, and update.
test0() async {
  for (var i = 0; i < 10; i = i + 1) {
    await p(i);
  }
  print('done');
}

test1() async {
  for (var i = await zero(); i < 10; i = i + 1) {
    print(i);
  }
  print('done');
}

test2() async {
  for (var i = 0; await compare(i); i = i + 1) {
    print(i);
  }
  print('done');
}

test3() async {
  for (var i = 0; i < 10; i = await inc(i)) {
    print(i);
  }
  print('done');
}

// Test initializer expression (not declaration).
test4() async {
  var i;
  for (i = 0; i < 10; i = i + 1) {
    await p(i);
  }
  print('done');
}

test5() async {
  var i;
  for (i = await zero(); i < 10; i = i + 1) {
    print(i);
  }
  print('done');
}

// Test missing initializer, condition, and update.
test6() async {
  var i = 0;
  for (; i < 10; i = i + 1) {
    await p(i);
  }
  print('done');
}

test7() async {
  for (var i = 0; ; i = i + 1) {
    if (i < 10) break;
    await p(i);
  }
  print('done');
}

test8() async {
  for (var i = 0; i < 10;) {
    await p(i);
    i = i + 1;
  }
  print('done');
}

test9() async {
  var i = 0;
  for(;;) {
    if (i < 10) break;
    await p(i);
    i = i + 1;
  }
  print('done');
}

// Test multiple declarations.
z() => 0;

test10() async {
  for (var i = z(), j = await zero(); i < 10; i = i + 1, j = j - 1) {
    await p(j);
  }
  print('done');
}

test11() async {
  for (var i = await zero(), j = z(); i < 10; i = i + 1, j = j - 1) {
    await p(j);
  }
  print('done');
}
