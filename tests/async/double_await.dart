import 'dart:async';

main() async {
   await (await f());
}

f() async {
  print('A');
}
