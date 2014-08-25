import 'dart:async';

main() async {
  await f();
}

f() async {
  var a = await g(42);
  var b = await g(4711);
  return a + b;
}

g(n) async {
  print(n); 
  return n; 
}
