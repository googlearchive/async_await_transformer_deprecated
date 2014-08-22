import 'dart:async';

main() async {
  var error, a;
  try {
    a = await f();
  } catch (e) {
    error = e;
  } finally {
    // prints “4711”
    if (error != null) print(error);
    if (a != null) print(a);
  }
}

f() async {
  try {
    await g('A');
    return 42;
  } finally {
    h('B');        // don’t await
    return 4711;
  }
}


g(n) async { 
  print(n); 
  return n; 
}

h(n) async { 
  print(n); 
  throw n; 
}
