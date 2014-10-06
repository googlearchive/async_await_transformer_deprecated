import 'dart:collection';

void main() {
  try {
    print(f());
  } catch (e) {
    print(e);
  }
}

f() sync* {
  try {
    yield 1;
    throw 2;
  } catch (e) {
    yield 3;
    throw 4;
  } finally {
    yield 5;
  }
}
