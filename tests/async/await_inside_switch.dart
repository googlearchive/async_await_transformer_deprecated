import 'dart:async';

main() async {
  var n = await f();
  print(n);
}

f() async {
  var n = 3;
  switch('a') {
    A: case 'a':
      print(await g('A'));
      continue B;
    B: case 'b':
      print(await g('B'));
       continue C;
    C: case 'c':
       print(await g('C'));
       if (n-- < 0) return 42;
       continue A;
  }
}

g(n) async => n;
