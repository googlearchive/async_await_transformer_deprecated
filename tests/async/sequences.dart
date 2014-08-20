f(s) {
  return new Future(() => print(s));
}

g(x, y, z) {}

test() async {
  g(print('a'), print('b'), print('c'));
  g(print('a'), print('b'), await f('c'));
  g(print('a'), await f('b'), print('c'));
  g(print('a'), await f('b'), await f('c'));
  g(await f('a'), print('b'), print('c'));
  g(await f('a'), print('b'), await f('c'));
  g(await f('a'), await f('b'), print('c'));
  g(await f('a'), await f('b'), await f('c'));
}
