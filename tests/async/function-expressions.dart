import 'dart:async';

id(x) async => x;

testAsync() async {
  var x = 42;
  ((x) => print(x))(x);
  ((x) { print(x); })(x);
  await ((x) async => print(x))(x);
  await ((x) async { print(x); })(x);
  await (() async {})();

  foo() async { print('foo'); }
  bar() async => 'bar';

  await foo();
  print(await bar());
}

testSync() {
  var x = 42;
  ((x) => print(x))(x);
  ((x) { print(x); })(x);
  ((x) async => print(x))(x);
  ((x) async { print(x); })(x);
  (() async {})();

  foo() async { print('foo'); }
  bar() async => 'bar';

  foo();
  bar();
}

nested() async {
  foo() {
    bar() async {
      print('Look Ma!');
    }
    return bar;
  }
  await foo()();
}
