import 'dart:async';

main() async {
  var a = await f();
  print(a);
}

f() async {
  return g();
}
  
g() async {
  return 42;
}
