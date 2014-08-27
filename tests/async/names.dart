// Test safe generation of names.
f(x) async => x;
g(x, y) => x;

main() async {
  var v0 = 0;
  var x0 = 1;
  g(v0, await f(x0));
}
