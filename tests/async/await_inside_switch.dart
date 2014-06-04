import 'dart:async';

main() async {
   var n = await f();
   print(n);
}

f() async {
  var n = 3;
  switch(“a”) {
    a: case "a": 
       print(await g(“A”)); 
       continue b;
    b: case "b": 
       print(await g(“B”)); 
       continue c;
    c: case "c": 
       print(await g(“C”)); 
       if(n— < 0) return 42;
       continue a;
  }
}

g(n) async { 
   return n; 
}