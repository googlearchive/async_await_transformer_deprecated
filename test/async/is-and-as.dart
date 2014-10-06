import 'dart:async';

f() async => 42;

main() async {
  print(await f() is int);
  print(await f() as int);
}
