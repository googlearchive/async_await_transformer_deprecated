import 'dart:async';

f() async => true;
g() async => () => true;

main() async {
  assert(await f());
  ;
  ;
  ;
  {}
  {}
  assert(await g());
}

