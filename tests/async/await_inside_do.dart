f(x) async => print(x);

g(n) async => n < 10;

test0() async {
  var x = 0;
  do {
    x = x + 1;
    await f(x);
  } while (await g(x));
  print('done');
}

test1() async {
  var x = 0;
  do {
    x = x + 1;
    if (x.isOdd) continue;
    await f(x);
  } while (await g(x));
  print('done');
}

test2() async {
  var x = 1;
  do {
    x = x + 1;
    if (x.isPrime) break;
    await f(x);
  } while (await g(x));
  print('done');
}
