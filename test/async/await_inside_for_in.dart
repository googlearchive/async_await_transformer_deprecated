p(x) async { print(x); }

id(x) async => x;

test0(ls) async {
  var x;
  for (x in ls) {
    await p(x);
  }
}

test1(ls) async {
  for (var x in ls) {
    await p(x);
  }
}

test2(ls) async {
  for (var x in await id(ls)) {
    print(x);
  }
}

test3(ls) async {
  for (var x in ls) {
    await p(x);
    if (x > 10) break;
  }
}

test4(ls) async {
  for (var x in ls) {
    if (x == 0) continue;
    await p(x);
  }
}
