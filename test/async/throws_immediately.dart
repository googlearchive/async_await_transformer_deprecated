import 'dart:async';

main() async {
  var passed = null;
  try {
    var a = f();
    passed = 0;
    await a;
  } catch (e) {
    if (passed == null) print(e);
  }
}

f() {
  throw 'boom';
}
