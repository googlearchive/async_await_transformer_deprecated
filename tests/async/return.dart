test0() async {
  await f();
  return 'a';
}

test1() async {
  return await f();
}

test2() async {
  return 'a';
  await f();
}
