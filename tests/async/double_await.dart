import 'dart:async';

import 'package:expect/expect.dart';

main() async {
   var x = await (await f());
   Expect.equals(x, 'A');
}

f() async => 'A';
