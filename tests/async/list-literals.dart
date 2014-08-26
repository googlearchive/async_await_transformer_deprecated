import 'dart:async';

f(n) async => n;

main() async {
  var x = 1, y = 2, z = 3;
  for (var e in [x, await f(y), z]) {
    print(e);
  }
}


  
