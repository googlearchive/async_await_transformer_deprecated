import 'dart:async';

t0() => true;
f0() => false;
t1() async => await t0();
f1() async => await f0();
p(x) async { print(x); }

test0() async {
  if (t0() && f0()) {
    await p(0);
  }
  if (await t1() && f0()) {
    await p(1);
  }
  if (t0() && await f1()) {
    await p(2);
  }
  if (await t1() && await f1()) {
    await p(3);
  }

  if (f0() || t0()) {
    await p(4);
  }
  if (await f1() || t0()) {
    await p(5);
  }
  if (f0() || await t1()) {
    await p(6);
  }
  if (await f1() || await t1()) {
    await p(7);
  }
}
