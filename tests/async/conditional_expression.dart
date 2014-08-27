import 'dart:async';

f(x) async { return x; }

main() async {
   var x = await f(true) ? 1 : 2;
   var y = true ? await f(1) : 2;
   var z = true ? 1 : await f(2);
   var q = true ? await f(1) : await f(2);
   var r = await(f(true)) ? await f(1) : await f(2);
}