import 'dart:async';

import 'package:unittest/unittest.dart';

main() async {
  var x = await (await f());
  expect(x, equals('A'));
}

f() async => 'A';
