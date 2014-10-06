f(n) async => n;

test0() async {
  var x = 0, y = 1, z;
  await f('done');
}

test1() async {
  var x = 0, y, z = await f(x);
  await f('done');
}

test2() async {
  var x = 0, y = await f(x), z;
  await f('done');
}

test3() async {
  var x = await f(0), y, z = 2;
  await f('done');
}
