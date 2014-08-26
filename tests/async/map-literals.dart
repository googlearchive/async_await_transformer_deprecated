import 'dart:async';

f(n) async => n;

main() async {
  var x = 1, y = 2, z = 3;
  var m = {'a': await f(x), 'b': y, 'c': z};
  var m1 = {'a': x, await f('b'): y, 'c': z};
  var m2 = {'a': await f(x), 'b': y, await f('c'): z};
  print(m['b'] == m1['b']);
  print(m[await f('b')] == (await f(m2))['b']);
}
