main() async* {
  for (var i in f()) {
    print(i);
  }
}

f() async* {
  yield 1;
  throw "boom";
}
