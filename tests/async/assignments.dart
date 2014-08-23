f(n) async { print(n); return n; }

g(x, y, z) {
  print(x);
  print(y);
  print(z);
}

test0() async {
  var x = 0;
  g(x, x = 1, await f(2));
}
