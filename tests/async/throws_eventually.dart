import 'dart:async';

main() async {
  var passed = false;
  try {
    var a = f();
    passed = true;
    await a;
  } catch(e) {
    if (passed) print(e);
  }
}

f() async {
  throw 'boom';
}
