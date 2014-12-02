import 'dart:async';

test() async {
  try {
    throw new Exception("boom!");
  } on Exception {
    print('☺');
  }
}

main() { test(); }
