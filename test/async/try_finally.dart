import 'dart:async';

f() async => print('f');

test() async {
  try {
    await f();
  } finally {
    print('finally');
  }
}

main() { test(); }
