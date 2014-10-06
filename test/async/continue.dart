import 'dart:async';

f(n) async => n;
p(x) async { print(x); }

test0() async {
  var i = 0;
  while (i < 10) {
    await f(++i);
    if (i.isOdd) continue;
    await p(i);
  }
}

test1() async {
  var i = 0;
  do {
    await f(++i);
    if (i.isOdd) continue;
    await p(i);
  } while (i < 10);
}

test2() async {
  for (var i = 0; i < 10; ++i) {
    await f(i);
    if (i.isOdd) continue;
    await p(i);
  }
}

test3() async {
  var i = 0;
L0: while (i < 10) {
    await f(++i);
    var j = 0;
 L1: while (j < 10) {
      await f(++j);
      if (j.isOdd) continue L0;
      await p(j);
    }
    await p(i);
  }
}

test4() async {
  var i = 0;
L0: do {
    await f(++i);
    var j = 0;
 L1: while (j < 10) {
      await f(++j);
      if (j.isOdd) continue L0;
      await p(j);
    }
    await p(i);
  } while (i < 10);
}

test5() async {
L0: for (var i = 0; i < 10; ++i) {
    await f(i);
    var j = 0;
 L1: while (j < 10) {
      await f(++j);
      if (j.isOdd) continue L0;
      await p(j);
    }
    await p(i);
  }
}

test6() async {
  var i = 0;
  switch (i) {
  L0: case 0:
    await p(i);
    continue L2;
  L1: case 1:
    await p(i);
    continue L0;
  L2: case 2:
    await p(i);
    continue L1;
  }
}
