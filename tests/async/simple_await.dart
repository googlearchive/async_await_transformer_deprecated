import 'dart:async';

main() async {
   await f();
}

f() async {
   print('A');
   var b = await g('B');
   print(b);
}

g(n) {
   return new Future(() => n);
}
