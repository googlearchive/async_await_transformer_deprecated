import 'dart:async';

f(s) {
  return new Future(() => print(s));
}

g(x, y, z) {}

test0() async {
  await f('before');
  try {
    print('in try');
  } catch (e) {
    print('in catch');
  }
  print('after');
}

test1() async {
  print('before');
  try {
    print('in try');
  } catch (e) {
    print('in catch');
  }
  await f('after');
}

test01() async {
  print('before');
  try {
    await f('in try');
  } catch (e) {
    print('in catch');
  }
  print('after');
}

test2() async {
  print('before');
  try {
    print('in try');
  } catch (e) {
    await f('in catch');
  }
  print('after');
}

test3() async {
  print('before');
  try {
    await f('in try');
  } catch (e) {
    await f('in catch');
  }
  print('after');
}

test4() async {
  await f('before');
  try {
    print('in try');
  } finally {
    print('in finally');
  }
  print('after');
}

test5() async {
  print('before');
  try {
    print('in try');
  } finally {
    print('in finally');
  }
  await f('after');
}

test6() async {
  print('before');
  try {
    await f('in try');
  } finally {
    print('in finally');
  }
  print('after');
}

test7() async {
  print('before');
  try {
    print('in try');
  } finally {
    await f('in finally');
  }
  print('after');
}

test8() async {
  print('before');
  try {
    await f('in try');
  } finally {
    await f('in finally');
  }
  print('after');
}

test9() async {
  await f('before');
  try {
    print('in try');
  } catch (e) {
    print('in catch');
  } finally {
    print('in finally');
  }
  print('after');
}

test10() async {
  print('before');
  try {
    print('in try');
  } catch (e) {
    print('in catch');
  } finally {
    print('in finally');
  }
  await f('after');
}

test11() async {
  print('before');
  try {
    await f('in try');
  } catch (e) {
    print('in catch');
  } finally {
    print('in finally');
  }
  print('after');
}

test12() async {
  print('before');
  try {
    print('in try');
  } catch (e) {
    await f('in catch');
  } finally {
    print('in finally');
  }
  print('after');
}

test13() async {
  print('before');
  try {
    print('in try');
  } catch (e) {
    print('in catch');
  } finally {
    await f('in finally');
  }
  print('after');
}

test14() async {
  print('before');
  try {
    await f('in try');
  } catch (e) {
    await f('in catch');
  } finally {
    await f('in finally');
  }
  await f('after');
}

test15() async {
  print('before');
  try {
    print('in try');
  } catch (e) {
    await f('in catch');
  } finally {
    await f('in finally');
  }
  print('after');
}

test16() async {
  print('before');
  try {
    await f('in try');
  } catch (e) {
    print('in catch');
  } finally {
    await f('in finally');
  }
  print('after');
}

test17() async {
  print('before');
  try {
    await f('in try');
  } catch (e) {
    await f('in catch');
  } finally {
    print('in finally');
  }
  print('after');
}
