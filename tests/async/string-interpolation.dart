import 'dart:async';

f(n) async => n;

main() async {
  var x = 1, y = 2, z = 3;
  print('x = ${await f(x)}, y = $y, z = ${await f(z)}');
}
